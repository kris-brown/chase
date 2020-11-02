module Lib
    ( Var, Val (..), Tup, Relation, RelName, RelAtom (..), Dict, Constraint, TGD, EGD, FD, FireInput, FDs,Inst, Counter,mkRel,mkInst,satTGD,satEGD,fireEqTup,fire,chase,project,query, someFunc
    ) where
import Data.Map as M
import Data.Text as T hiding (all, zip)
import Data.Set as Set
import Control.Exception (assert)
import Control.Monad (foldM)
import Control.Lens

-- Values are either constants (let's restrict to strings) or variables (int indexed)
type Var = Int
data Val = C String  | V Var deriving (Show, Eq, Ord)
type Tup = [Val]
type Relation = [Tup]
type RelName = T.Text
data RelAtom = RelAtom {name::RelName, rel::Relation} deriving (Show, Eq, Ord)

type Dict = M.Map Var Val
-- These tuples are allowed to have variables
type Constraint = [RelAtom]

-- Tuple-generating dependency
data TGD = TGD {alpha::Constraint, beta:: Constraint}

-- Equality-generating dependency
newtype EGD = EGD {egd::Constraint} -- implicit: asserting equality over vars 'a' 'b'

-- Functional dependency
type FD = Either TGD EGD
type FireInput = Either ([Var], [Tup]) -- create these tuples
                        (Var,   Val)   -- overwrite this var with this val
type FDs = Set.Set FD

type Inst =  (M.Map RelName Relation)

type Counter = Int

------------------------------------------------------
makeLenses ''Val
makeLenses ''RelAtom
------------------------------------------------------


-- Safely make a relation; all tuples should have the same length
mkRel :: [Tup] -> Relation
mkRel []      = []
mkRel l@(h:t) = assert (isValid t) $ l
    where headlength   = Prelude.length  h
          isValid = all (\x -> Prelude.length x == headlength)

-- Safely make an instance.
mkInst :: [(RelName, [[Val]])] -> Inst
mkInst = M.fromList . Prelude.map (\(n, valslist) ->
            (n, mkRel valslist))

satTGD :: TGD -> Inst -> Maybe FireInput
satTGD = undefined

satEGD :: EGD -> Inst -> Maybe FireInput
satEGD = undefined

-- Fire an equality constraint (unify two vars, if both const, then fail)
fireEqTup :: Inst -> RelAtom -> Maybe Inst
fireEqTup i r = undefined

-- Counter lets us know what variable names are free
fire :: Constraint -> Inst -> FireInput -> Counter -> Maybe Inst
fire c i (Left (varnums, tups)) j = undefined
fire c i (Right (var, val))    _ = substitute i var val

substitute inst var val = undefined


-- Helper for checkFD operating on a list of FDs
checkFD_ :: [FD] -> Inst -> Maybe FireInput
checkFD_ []    _ = Nothing
checkFD_ (h:t) i = case (either satTGD satEGD) h i of
                        Nothing -> checkFD_ t i
                        Just x  -> Just x

-- Short circuiting check of FDs (stops and returns FireInput once one fails)
checkFD :: FDs -> Inst -> Maybe FireInput
checkFD fds i = checkFD_ (Set.toList fds) i

-- The chase could fail or fail to terminate
chase :: FDs -> Inst -> Maybe Inst
chase fds i = undefined

-- Collect all values of a particular attribute
project :: Relation -> Int -> [Val]
project r a = Prelude.map ((!! a) ) r


-- Check if a given row matches a constraint.
-- Keep track of what variables are unified with which values
queryTupTup :: Tup -> Dict -> Tup -> Maybe Dict
queryTupTup constraint dict tup = foldM f dict $ zip constraint tup
  where f dict vals = case vals of
                        (C x, C y) -> if x == y then Just dict else Nothing
                        (V x,   y) -> update x y
                        (y,   V x) -> update x y
        update :: Var -> Val -> Maybe Dict
        update x y = case M.lookup x dict of
            Nothing -> Just $ M.insert x y dict
            Just z -> if y == z then Just dict else Nothing

queryTupRel :: Tup -> Dict -> Relation -> Maybe Dict
queryTupRel constraint = foldM (queryTupTup constraint)

query :: Constraint -> Inst -> Maybe (Dict, Inst)
query constraint  = foldM f (M.empty, [])
  where f (dict, res) _ = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"


