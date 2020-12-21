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

data Atom = Atom {rel :: RelName, tup :: Tup} deriving (Eq, Ord)

data Query = Query {unQuery :: Set Atom, distinguished :: Set Var} deriving (Eq, Ord)

-- Tuple-generating dependency
data TGD = TGD {alpha :: Query, beta :: Set Atom}

-- Equality-generating dependency
newtype EGD = EGD {egd :: Query} -- implicit: asserting equality over vars 'a' 'b'

-- Functional dependency
type FD = Either TGD EGD

type FireInput =
  Either
    ([Var], [Tup]) -- create these tuples
    (Var, Val) -- overwrite this var with this val

type FDs = Set.Set FD

type Counter = Int

------------------------------------------------------
-- Parsing
atomFromList :: String -> [String] -> Atom
atomFromList n xs = Atom (T.pack n) (tupFromList xs)

------------------------
-- MISC --
----------

--
matchTup :: Tup -> Tup -> Maybe Tup
matchTup x y = matchDict x y >>= (\dict -> pure $ tSub dict x)

matchDict :: Tup -> Tup -> Maybe Dict
matchDict x y = foldM f M.empty $ V.zip x y
  where
    f d (V a, b) = case M.lookup a d of
      Nothing -> Just (M.insert a b d)
      Just c -> if b /= c then Nothing else Just d
    f d (C a, b) = if C a == b then Just d else Nothing

tSub :: Dict -> Tup -> Tup
tSub d =
  V.map
    ( \x -> case x of
        C _ -> x
        V i -> M.findWithDefault x i d
    )

matchRel :: Atom -> Relation -> [Var] -> Relation
matchRel (Atom _ tupl) (Relation _ _ tupls) dist = Relation "" colnames res
  where
    cols = V.filter (`P.elem` P.map V dist) tupl
    colnames = V.map varToColName cols
    res = Set.fromList $ MB.mapMaybe (matchTup tupl) $ Set.toList tupls

-- Fix this so that it runs on an instance and feeds matchRel the right Rel
run :: Query -> Relation -> Relation
run (Query matchs dist) r = joins (P.map f $ Set.toList matchs) True
  where
    f atom = matchRel atom r $ Set.toList dist

isVar :: Val -> Maybe Var
isVar (C _) = Nothing
isVar (V x) = Just x

tupVars :: Tup -> Vector Var
tupVars = V.mapMaybe isVar

atomVars :: Atom -> Set Var
atomVars =
  Set.fromList
    . MB.mapMaybe
      ( \case
          C _ -> Nothing
          V v -> Just v
      )
    . V.toList
    . tup

qFromAtoms :: [Atom] -> Query
qFromAtoms xs = Query xs' allvar
  where
    xs' = Set.fromList xs
    allvar = Set.unions $ Set.map atomVars xs'

------------------------
-- Dependency related --
------------------------
--Smart constructor
mkEGD :: Query -> EGD
mkEGD q
  | distinguished q /= Set.fromList [0, 1] = error "EGD query must have distinguished 0 and 1"
  | otherwise = EGD q

fireEGD :: Relation -> EGD -> Maybe Relation
fireEGD r (EGD q) = substitute r <$> subdict
  where
    res = Set.map (\x -> (x V.! 0, x V.! 1)) $ tups $ run q r
    subdict = foldM fireEGDfold M.empty res

-- Make a variable point to a value. Also, in case othre things point to that variable, replace all things that point to it with a pointer the new target
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

fireTGD :: TGD -> Relation -> Counter -> (Relation, Counter)
fireTGD (TGD q ts) r c = (r {tups = Set.union (tups r) newtups}, c')
  where
    res = run q r
    dist = distinguished q
    columnMap = M.fromSet (MB.fromJust . flip V.elemIndex (attrs res) . varToColName . V) dist
    dicts = Set.map (\t -> M.map (t V.!) columnMap) (tups res)
    (newtups, c') = L.foldl' (tgdMapFunc (Set.toList ts)) (Set.empty, c) $ Set.toList dicts

type TGDstate = (Set Tup, Int)

-- Generate a tuple and possibly update the counter
-- Do this for every Atom in the TGD consequent clause
tgdMapFunc :: [Atom] -> TGDstate -> Dict -> TGDstate
tgdMapFunc atoms tgdstate t = L.foldl' (tgdMapFunc' t) tgdstate atoms

-- Generate a tuple and possibly update the counter
-- for a particular Atom in the TGD consequent clause
tgdMapFunc' :: Dict -> TGDstate -> Atom -> TGDstate
tgdMapFunc' mapping (oldTups, counter) atom = (Set.insert newTups oldTups, counter + nUnbound)
  where
    atomTup = tup atom
    isUnbound v = not $ M.member v mapping
    unbound = V.indexed $ V.fromList $ Set.toList $ Set.fromList $ V.toList $ V.filter isUnbound $ tupVars atomTup
    unboundMap = M.fromList $ V.toList $ V.map (\(var, val) -> (val, V (var + counter))) unbound
    newTups = tupSubstitute (M.union mapping unboundMap) atomTup
    nUnbound = V.length unbound

substitute :: Relation -> Dict -> Relation
substitute (Relation n a t) d = Relation n a (Set.map (tupSubstitute d) t)

tupSubstitute :: Dict -> Tup -> Tup
tupSubstitute = V.map . valSubstitute

valSubstitute :: Dict -> Val -> Val
valSubstitute d (V i) = M.findWithDefault (V i) i d
valSubstitute _ x = x

--The chase could fail or fail to terminate
chase :: FDs -> Inst -> Maybe Inst
chase = undefined
