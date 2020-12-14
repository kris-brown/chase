module Lib
  ( Var,
    Val (..),
    Tup,
    Relation,
    RelName,
    Atom (..),
    Dict,
    TGD (..),
    EGD (..),
    FD,
    FireInput,
    FDs,
    Inst,
    Counter,
    fromLists,
    rename,
    relFromLists,
    fromString,
    fromCSV,
    someFunc,
    atomFromList,
    join,
    joins,
  )
where

import Data.List as L
import Data.Map as M
import Data.Maybe (fromJust)
import Data.Set as Set
import Data.Text as T hiding (all, replicate, zip)
import Data.Validity (Validity (..), trivialValidation)
import Data.Vector as V hiding (foldM, replicate, (++), (//))
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getHomeDirectory)
import Text.CSV (parseCSVFromFile)
import Text.PrettyPrint.Boxes as Box
import Text.Read (readMaybe)
import Prelude as P hiding (all)

-- Values are either constants (let's restrict to strings) or variables (int indexed)
type Var = Int

data Val = C Text | V Var deriving (Eq, Ord, Generic)

instance Validity Val where validate = trivialValidation

type Tup = Vector Val

type AttrName = T.Text

type RelName = T.Text

data Relation = Relation
  { name :: RelName,
    attrs :: Vector AttrName,
    tups :: Set Tup
  }
  deriving (Eq, Ord)

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

type Inst = Set Relation

type Counter = Int

------------------------------------------------------
--import Control.Lens ()
--makeLenses ''Val
--makeLenses ''Atom
------------------------------------------------------
------------------------
-- Printing functions --
------------------------
-- (Box, char, hcat, hsep, left, render, text, top, vcat, vsep, (//))
pad :: Int -> String -> String
pad width x = x ++ replicate k ' '
  where
    k = width - P.length x

fmtCol :: [String] -> Box.Box
fmtCol items = Box.vcat Box.left [hsep, Box.vcat Box.left (L.intersperse hsep (Box.text . pad width <$> items)), hsep]
  where
    width = P.maximum $ P.length <$> items
    hsep = Box.text (L.replicate width '-')

table :: [[String]] -> Box.Box
table rows = Box.hcat Box.top [vsep, Box.hcat Box.top (L.intersperse vsep (P.map fmtCol columns)), vsep]
  where
    columns = L.transpose rows
    nrows = P.length rows
    vsep = vcat Box.left $ Box.char <$> ("+" ++ P.concat (L.replicate nrows "|+"))

instance Show Val where
  show (V i) = show i
  show (C x) = T.unpack x

instance Show Relation where
  show (Relation name attrs tups) =
    P.concat
      [ T.unpack name,
        "\n",
        Box.render $ table $ (T.unpack <$> V.toList attrs) : P.map (P.map show . V.toList) (Set.toList tups)
      ]

-----------------------
-- Parsing functions --
-----------------------

-- Parse numeric strings as integer IDs, otherwise Constant data
fromString :: String -> Val
fromString x = maybe (C $ T.pack x) V (readMaybe x)

-- Parse list of list of strings interpreted as Vals
tupFromList :: [String] -> Tup
tupFromList = V.fromList . P.map fromString

fromLists :: [[String]] -> Set Tup
fromLists = Set.fromList . P.map tupFromList

relFromLists :: String -> [String] -> [[String]] -> Relation
relFromLists name attr tups = Relation (T.pack name) (V.fromList $ P.map T.pack attr) (fromLists tups)

-- mkInst :: [(String, [String], [[String]])] -> Inst
-- mkInst = Set.fromList . P.map (uncurry3 relFromLists)

fromCSV :: FilePath -> IO Relation
fromCSV fp = do
  dfault <- (\x -> P.concat [x, "/chase/data/", fp, ".csv"]) <$> getHomeDirectory
  check <- doesFileExist dfault
  let fp' = if check then dfault else fp
  parseRes <- parseCSVFromFile fp'
  return $ case parseRes of
    Left _ -> error ("Can't find " ++ fp)
    Right (header : rows) -> relFromLists fp header rows

atomFromList :: String -> [String] -> Atom
atomFromList n xs = Atom (T.pack n) (tupFromList xs)

------------------------
instance Semigroup Relation where
  a <> b = join False a b

vToSet :: Ord a => Vector a -> Set a
vToSet = Set.fromList . V.toList

join :: Bool -> Relation -> Relation -> Relation
join saveOverlap (Relation _ nAttr nTup) (Relation _ mAttr mTup) =
  Relation "" (V.fromList (overlapAttr ++ resattr)) (Set.fromList tup)
  where
    allattr = Set.fromList (V.toList nAttr ++ V.toList mAttr)
    overlap = Set.intersection (vToSet nAttr) (vToSet mAttr)
    overlapAttr = if not saveOverlap then [] else Set.toList overlap
    resattr = Set.toList $ Set.difference allattr overlap
    getinds x = P.map (\i -> fromJust $ L.elemIndex i $ V.toList x) $ Set.toList overlap
    inds = P.zip (getinds nAttr) (getinds mAttr)
    attrOnly x = Set.toList $ Set.difference (vToSet x) overlap
    newinds = (if saveOverlap then getinds nAttr else []) ++ [fromJust $ L.elemIndex a $ V.toList nAttr | a <- attrOnly nAttr]
    mNewInds = [fromJust $ L.elemIndex a $ V.toList mAttr | a <- attrOnly mAttr]
    merge x y = V.fromList $ [x V.! i | i <- newinds] ++ [y V.! i | i <- mNewInds]
    tup = [merge nt mt | nt <- Set.toList nTup, mt <- Set.toList mTup, (i, j) <- inds, nt V.! i == mt V.! j]

rEmpty :: Relation
rEmpty = Relation "" V.empty Set.empty

joins :: [Relation] -> Bool -> Relation
joins [] _ = rEmpty
joins [x] _ = x
joins (h : t) sO = join sO h $ joins t sO

rename :: Relation -> Map AttrName AttrName -> Relation
rename (Relation n a t) m = Relation n a' t
  where
    a' = V.map (\x -> M.findWithDefault x x m) a

matchRel :: Atom -> Relation -> Set Var -> Relation
matchRel = undefined

run :: Query -> Relation -> Relation
run (Query matchs dist) r = joins (P.map f $ Set.toList matchs) True
  where
    f atom = matchRel atom r dist

------------------------
-- Dependency related --
------------------------
satTGD :: TGD -> Inst -> Maybe FireInput
satTGD = undefined

satEGD :: EGD -> Inst -> Maybe FireInput
satEGD = undefined

-- Fire an equality constraint (unify two vars, if both const, then fail)
-- fireEqTup :: Inst -> Atom -> Maybe Inst
-- fireEqTup i r = undefined

-- Counter lets us know what variable names are free
-- fire :: Constraint -> Inst -> FireInput -> Counter -> Maybe Inst
-- fire c i (Left (varnums, tups)) j = undefined
-- fire c i (Right (var, val)) _ = substitute i var val

substitute inst var val = undefined

-- Helper for checkFD operating on a list of FDs
-- checkFD_ :: [FD] -> Inst -> Maybe FireInput
-- checkFD_ [] _ = Nothing
-- checkFD_ (h : t) i = case either satTGD satEGD h i of
--   Nothing -> checkFD_ t i
--   Just x -> Just x

-- Short circuiting check of FDs (stops and returns FireInput once one fails)
-- checkFD :: FDs -> Inst -> Maybe FireInput
-- checkFD fds = checkFD_ (Set.toList fds)

-- The chase could fail or fail to terminate
-- chase :: FDs -> Inst -> Maybe Inst
-- chase fds i = undefined

-- Check if a given row matches a constraint.
-- Keep track of what variables are unified with which values
-- queryTupTup :: Tup -> Dict -> Tup -> Maybe Dict
-- queryTupTup constraint dict tup = foldM f dict $ zip constraint tup
--   where f dict vals = case vals of
--                         (C x, C y) -> if x == y then Just dict else Nothing
--                         (V x,   y) -> update x y
--                         (y,   V x) -> update x y
--         update :: Var -> Val -> Maybe Dict
--         update x y = case M.lookup x dict of
--             Nothing -> Just $ M.insert x y dict
--             Just z -> if y == z then Just dict else Nothing

-- queryTupRel :: Tup -> Dict -> Relation -> Maybe Dict
-- queryTupRel constraint = foldM (queryTupTup constraint)

-- query :: Constraint -> Inst -> Maybe (Dict, Inst)
-- query constraint i = undefined -- foldM f i (M.empty, [])
--   where f (dict, res) _ = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"
