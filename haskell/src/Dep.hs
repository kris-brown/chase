module Dep
  ( Atom (..),
    Dict,
    Query (..),
    TGD (..),
    EGD (..),
    mkEGD,
    FD,
    FireInput,
    FDs,
    Counter,
    atomFromList,
    fireTGD,
    run,
    qFromAtoms,
    matchRel,
    matchDict,
    matchTup,
    fireEGD,
    chase,
    qRename,
  )
where

------------------------------------------------------
-------------
-- Imports --
-------------

--import Debug.Trace (trace)

import Control.Monad (foldM)
import Data.List as L
import Data.Map as M
import Data.Maybe as MB
import Data.Set as Set
import Data.Text as T
import Data.Vector as V hiding (foldM)
import Rel
import Prelude as P

------------------------------------------------------
-----------
-- Types --
-----------

type Dict = M.Map Var Val

-- A pattern to be matched to tuples of a particular relation
data Atom = Atom {rel :: RelName, tup :: Tup} deriving (Eq, Ord)

-- Conjunctive query. A logical constraint with variables which, when the
-- query is satisfied, get matched to values.
-- Give distinguished variables (ones quantified over)
-- Running the query returns all satisfying matches.
data Query = Query {unQuery :: Set Atom, distinguished :: Set Var} deriving (Eq, Ord)

-- Tuple-generating dependency
data TGD = TGD {alpha :: Query, beta :: Set Atom}

-- Equality-generating dependency
newtype EGD = EGD {egd :: Query} -- implicit: asserting equality over vars 0 1

-- Functional dependency
type FD = Either TGD EGD

type FDs = Set.Set FD

type Counter = Int

------------------------------------------------------
-- Parsing
atomFromList :: String -> [String] -> Atom
atomFromList n xs = Atom (T.pack n) (tupFromList xs)

------------
-- Queries -
------------
qRename :: Query -> Map RelName RelName -> Query
qRename q m = q {unQuery = Set.map f (unQuery q)}
  where
    f (Atom r t) = Atom (M.findWithDefault r r m) t

------------------------
-- MISC --
----------

-- For each distinguished variable in `vals`
-- (which must be found in the pattern tuple; it's up to matchRel to enforce this)
--
matchTup :: Vector Var -> Tup -> Tup -> Maybe Tup
matchTup vals pattrn potentialMatch =
  matchDict pattrn potentialMatch >>= (\dict -> pure $ lookUp dict vals)
  where
    lookUp d = V.map (d M.!)

-- Given a pattern (with variables) and a target, find a mapping
-- of its variables that yields the target tuple, if possible
matchDict :: Tup -> Tup -> Maybe Dict
matchDict pattrn potentialMatch = foldM f M.empty $ V.zip pattrn potentialMatch
  where
    f d (V a, b) = case M.lookup a d of
      Nothing -> Just (M.insert a b d)
      Just c -> if b /= c then Nothing else Just d
    f d (C a, b) = if C a == b then Just d else Nothing

-- For some list of distinguished variables (found in an atom's tuple)
-- create a relation by trying to match that tuple with each row of a
-- given relation
matchRel :: Atom -> Inst -> [Var] -> Relation
matchRel (Atom aRel aTupl) (Inst i) dist = Relation "" colnames res
  where
    tupls = tups $ i M.! aRel
    cols = V.filter (`P.elem` P.map V dist) aTupl
    colvals = V.mapMaybe isVar cols
    colnames = V.map varToColName cols
    res = Set.fromList $ MB.mapMaybe (matchTup colvals aTupl) $ Set.toList tupls

-- Fix this so that it runs on an instance and feeds matchRel the right Rel
run :: Query -> Inst -> Relation
run (Query matchs dist) i = project res newcols
  where
    f atom = matchRel atom i $ Set.toList dist
    res = joins (P.map f $ Set.toList matchs) True
    newcols = P.map (varToColName . V) $ Set.toList dist

isVar :: Val -> Maybe Var
isVar (C _) = Nothing
isVar (V x) = Just x

tupVars :: Tup -> Vector Var
tupVars = V.mapMaybe isVar

qFromAtoms :: [Atom] -> Query
qFromAtoms xs = Query xs' allvar
  where
    xs' = Set.fromList xs
    allvar = Set.unions $ Set.map atomVars xs'
    atomVars = Set.fromList . V.toList . tupVars . tup

------------------------
-- Dependency related --
------------------------
--Smart constructor
mkEGD :: Query -> EGD
mkEGD q
  | distinguished q /= Set.fromList [0, 1] = error "EGD query must have distinguished 0 and 1"
  | otherwise = EGD q

fireEGD :: Inst -> EGD -> Maybe Inst
fireEGD i (EGD q) = flip substitute i <$> subdict
  where
    res = Set.map (\x -> (x V.! 0, x V.! 1)) $ tups $ run q i
    subdict = foldM fireEGDfold M.empty res

-- Make a variable point to a value. Also, in case other things point to that variable,
-- replace all things that point to it with a pointer the new target
pointTo :: Var -> Val -> Dict -> Dict
pointTo i v = M.insert i v . M.map f
  where
    f val = if val == V i then v else val

fireEGDfold :: Dict -> (Val, Val) -> Maybe Dict
fireEGDfold d (C x, C y) = if x == y then Just d else Nothing
fireEGDfold d (V x, C y) = case M.lookup x d of
  Nothing -> Just $ pointTo x (C y) d
  Just z -> if C y == z then Just $ pointTo x z d else Nothing
fireEGDfold d (C y, V x) = fireEGDfold d (V x, C y)
fireEGDfold d (V x, V y) = case (M.lookup x d, M.lookup y d) of
  (Nothing, Nothing) -> Just $ pointTo x (V y) d
  (Just a, Nothing) -> Just $ pointTo y a d
  (Nothing, Just a) -> Just $ pointTo x a d
  (Just (V a), Just z) -> Just $ pointTo a z d
  (Just z, Just (V a)) -> Just $ pointTo a z d
  (Just (C a), Just (C b)) -> if a == b then Just d else Nothing

fireTGD :: TGD -> Inst -> Counter -> (Inst, Counter)
fireTGD (TGD q ts) i c = result --(r {tups = Set.union (tups r) newtups}, c')
  where
    res = run q i
    dist = distinguished q
    columnMap = M.fromSet (MB.fromJust . flip V.elemIndex (attrs res) . varToColName . V) dist
    dicts = Set.map (\t -> M.map (t V.!) columnMap) (tups res)
    result = L.foldl' (tgdMapFunc (Set.toList ts)) (i, c) $ Set.toList dicts

type TGDstate = (Inst, Int)

-- Generate a tuple and possibly update the counter
-- Do this for every Atom in the TGD consequent clause
tgdMapFunc :: [Atom] -> TGDstate -> Dict -> TGDstate
tgdMapFunc atoms tgdstate t = L.foldl' (tgdMapFunc' t) tgdstate atoms

-- Generate a tuple and possibly update the counter
-- for a particular Atom in the TGD consequent clause
tgdMapFunc' :: Dict -> TGDstate -> Atom -> TGDstate
tgdMapFunc' mapping (oldInst, counter) atom = (addTup oldInst (rel atom) newTups, counter + nUnbound)
  where
    atomTup = tup atom
    isUnbound v = not $ M.member v mapping
    unbound = V.indexed $ V.fromList $ Set.toList $ Set.fromList $ V.toList $ V.filter isUnbound $ tupVars atomTup
    unboundMap = M.fromList $ V.toList $ V.map (\(var, val) -> (val, V (var + counter))) unbound
    newTups = tupSubstitute (M.union mapping unboundMap) atomTup
    nUnbound = V.length unbound

addTup :: Inst -> RelName -> Tup -> Inst
addTup (Inst i) rn t = Inst $ M.updateAt upd (M.findIndex rn i) i
  where
    upd _ r = Just $ r {tups = Set.insert t (tups r)}

relSubstitute :: Dict -> Relation -> Relation
relSubstitute d (Relation n a t) = Relation n a (Set.map (tupSubstitute d) t)

-- Apply a mapping of variables to values to an instance
substitute :: Dict -> Inst -> Inst
substitute d = Inst . M.map (relSubstitute d) . rels

tupSubstitute :: Dict -> Tup -> Tup
tupSubstitute = V.map . valSubstitute

valSubstitute :: Dict -> Val -> Val
valSubstitute d (V i) = M.findWithDefault (V i) i d
valSubstitute _ x = x

--The chase could fail or fail to terminate
chase :: FDs -> Inst -> Maybe Inst
chase = undefined
