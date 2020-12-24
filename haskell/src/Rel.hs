module Rel
  ( Var,
    Val (..),
    Tup,
    Relation (..),
    RelName,
    Inst (..),
    mkInst,
    genRel,
    fromRels,
    instUnion,
    iEmpty,
    fromLists,
    renameAttrs,
    relFromLists,
    fromString,
    fromCSV,
    join,
    joins,
    varToColName,
    tupFromList,
    project,
  )
where

------------------------------------------------------
-------------
-- Imports --
-------------

import Data.List as L
import Data.Map as M
import Data.Maybe as MB
import Data.Set as Set
import Data.Text as T
import Data.Tuple.Extra (uncurry3)
import Data.Validity (Validity (..), check, trivialValidation)
import Data.Vector as V hiding (uniq)
--import Debug.Trace (trace)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getHomeDirectory)
import Test.QuickCheck (Arbitrary (..), Gen, arbitrary, elements, genericShrink, oneof, suchThat, vectorOf)
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Vector ()
import Test.Validity (GenUnchecked (..), GenValid (..), upTo)
import Text.CSV (parseCSVFromFile)
import Text.PrettyPrint.Boxes as Box hiding ((<>))
import Text.Read (readMaybe)
import Prelude as P

------------------------------------------------------
-----------
-- Types --
-----------

-- Values are either string constants or variables (int-indexed)
type Var = Int

data Val = C Text | V Var deriving (Eq, Ord, Generic)

-- Tuples are implemented as vectors: a fixed sequence of Vals
type Tup = Vector Val

-- Relations are a named set of tuples, with named attributes
type AttrName = T.Text

type RelName = T.Text

data Relation = Relation
  { name :: RelName,
    attrs :: Vector AttrName,
    tups :: Set Tup
  }
  deriving (Eq, Ord, Generic)

-- Instances are sets of relations
newtype Inst = Inst {rels :: Map RelName Relation} deriving (Eq, Ord, Generic)

------------------------------------------------------
--import Control.Lens ()
--makeLenses ''Val
--makeLenses ''Atom

------------------------------------------------------------
---------------
-- Instances --
---------------

uniq :: Ord a => [a] -> Bool
uniq xs = P.length xs == Set.size (Set.fromList xs)

instance Arbitrary Val where
  arbitrary = oneof [C . pack . return <$> elements ['a' .. 'z'], V <$> upTo 5]
  shrink (C x) = [C "a" | x /= "a"]
  shrink (V x) = [V (x `div` 2)]

instance Validity Val where validate = trivialValidation

instance Validity Relation where
  validate r =
    mconcat
      [ check (val' (tups r)) "All tuples have the same length as the # of attrs.",
        check (uniq ats) "All attrs should have unique names"
      ]
    where
      ats = V.toList $ attrs r
      n = P.length ats
      val' ts = P.and $ Set.map ((== n) . V.length) ts

genTup :: Int -> Gen Tup
genTup n = V.fromList <$> vectorOf n (arbitrary :: Gen Val)

genRel :: Int -> Gen Relation
genRel nattr = do
  n <- pack <$> arbitrary
  a <- P.map pack <$> vectorOf nattr arbitrary `suchThat` uniq
  ntup <- upTo 10
  t <- Set.fromList <$> vectorOf ntup (genTup nattr)
  return (Relation n (V.fromList a) t)

instance GenValid Relation where
  genValid = do
    n <- upTo 3
    genRel n

  shrinkValid = genericShrink -- either remove col or remove row or simplify name without causing a collision

instance Arbitrary Relation where
  arbitrary = genValid
  shrink = genericShrink

instance GenUnchecked Relation where
  genUnchecked = genValid
  shrinkUnchecked = genericShrink

instance Validity Inst where
  validate (Inst rs) =
    check
      (M.empty == M.filterWithKey (\k r -> k /= name r) rs)
      "All keys point to relations with that as the name"

-- Join is an associative operation
instance Semigroup Relation where
  a <> b = join False a b

-- instunion is an associative operation
instance Semigroup Inst where
  (<>) = instUnion

instance Show Val where
  show (V i) = show i
  show (C x) = T.unpack x

-- View the relation as a table
instance Show Relation where
  show (Relation relname relattrs reltups) =
    P.concat
      [ T.unpack relname,
        "\n",
        Box.render $ table $ (T.unpack <$> V.toList relattrs) : P.map (P.map show . V.toList) (Set.toList reltups)
      ]

instance Show Inst where
  show (Inst i) = L.intercalate "\n\n" (P.map show (M.elems i))

------------------------------------------------------------
------------------------
-- Printing functions --
------------------------
pad :: Int -> String -> String
pad width x = x <> P.replicate k ' '
  where
    k = width - P.length x

fmtCol :: [String] -> Box.Box
fmtCol items = Box.vcat Box.left [hsep', Box.vcat Box.left (L.intersperse hsep' (Box.text . pad width <$> items)), hsep']
  where
    width = P.maximum $ P.length <$> items
    hsep' = Box.text (L.replicate width '-')

table :: [[String]] -> Box.Box
table rows' = Box.hcat Box.top [vsep', Box.hcat Box.top (L.intersperse vsep' (P.map fmtCol columns')), vsep']
  where
    columns' = L.transpose rows'
    nrows = P.length rows'
    vsep' = vcat Box.left $ Box.char <$> ("+" <> P.concat (L.replicate nrows "|+"))

------------------------------------------------------------
-----------------------
-- Parsing functions --
-----------------------

-- Parse numeric strings as integer IDs, otherwise interpret as Constant data
fromString :: String -> Val
fromString x = maybe (C $ T.pack x) V (readMaybe x)

-- Parse list of strings interpreted as Vals
tupFromList :: [String] -> Tup
tupFromList = V.fromList . P.map fromString

-- Parse list of list of strings interpreted as Vals
fromLists :: [[String]] -> Set Tup
fromLists = Set.fromList . P.map tupFromList

-- Friendly constructor for relation
relFromLists :: String -> [String] -> [[String]] -> Relation
relFromLists relname attr reltups = Relation (T.pack relname) (V.fromList $ P.map T.pack attr) (fromLists reltups)

-- Friendly constructor for instances
mkInst :: [(String, [String], [[String]])] -> Inst
mkInst = fromRels . P.map (uncurry3 relFromLists)

-- Parse a CSV file. Path is default as interpreted as the name
-- of a file in the /data/ folder, treated as absolute if that's
-- not successful
fromCSV :: FilePath -> IO Relation
fromCSV filepath = do
  dfault <- (\home -> P.concat [home, "/chase/data/", filepath, ".csv"]) <$> getHomeDirectory
  fcheck <- doesFileExist dfault
  let filepath' = if fcheck then dfault else filepath
  parseResult <- parseCSVFromFile filepath'
  return $ case parseResult of
    Left _ -> error ("Can't find/parse " <> filepath)
    Right (header : rows') -> relFromLists filepath header rows'
    Right _ -> error ("Can't find/parse " <> filepath)

------------------------------------------------------------
-------------------------------
-- Basic relation operations --
-------------------------------
--Canonical empty relation
-- rEmpty :: Relation
-- rEmpty = Relation "" V.empty Set.empty

-- Rename attrs with a map
renameAttrs :: Relation -> Map AttrName AttrName -> Relation
renameAttrs rel attrMap = rel {attrs = updated}
  where
    updated = V.map (\attr -> M.findWithDefault attr attr attrMap) $ attrs rel

reorder :: Vector Int -> Tup -> Tup
reorder = flip V.backpermute

project :: Relation -> [AttrName] -> Relation
project (Relation rname rattrs rtup) newattr = Relation rname (V.fromList newattr) newtup
  where
    neworder = V.fromList $ getinds rattrs newattr
    newtup = Set.map (reorder neworder) rtup

------------------------------------------------------------
-- JOIN --
----------
-- Take a list of needles and a vector haystack and return, for each needle
-- the index in the haystack. Throws an error if not found
getinds :: Eq a => Vector a -> [a] -> [Int]
getinds haystack = P.map (\i -> MB.fromJust $ L.elemIndex i $ V.toList haystack)

-- The merge step can be made faster with V.backpermute(unsafe)
join :: Bool -> Relation -> Relation -> Relation
join saveOverlap (Relation _ nAttr nTup) (Relation _ mAttr mTup) =
  Relation "" (V.fromList (overlapAttr P.++ nonOverlapAttr)) (Set.fromList tup)
  where
    allattr = Set.fromList (V.toList nAttr <> V.toList mAttr)
    overlap = Set.toList $ Set.intersection (vToSet nAttr) (vToSet mAttr)
    overlapAttr = if not saveOverlap then [] else overlap
    nonOverlapAttr = Set.toList $ Set.difference allattr $ Set.fromList overlap
    inds = P.zip (getinds nAttr overlap) (getinds mAttr overlap)
    attrOnly x = Set.toList $ Set.difference (vToSet x) $ Set.fromList overlap
    newinds = V.fromList $ getinds nAttr (overlapAttr <> attrOnly nAttr)
    mNewInds = V.fromList $ getinds mAttr $ attrOnly mAttr
    merge x y = reorder newinds x <> reorder mNewInds y
    tup = [merge nt mt | nt <- Set.toList nTup, mt <- Set.toList mTup, P.and [nt V.! i == mt V.! j | (i, j) <- inds]]

joins :: [Relation] -> Bool -> Relation
joins [] _ = error "Should not call joins on empty list" -- never enter this case
joins [x] _ = x
joins (h : t) sO = join sO h $ joins t sO

------------------------------------------------------------
----------
-- INSTS --
-----------
fromRels :: [Relation] -> Inst
fromRels = Inst . M.fromList . P.map (\rel -> (name rel, rel))

instUnion :: Inst -> Inst -> Inst
instUnion (Inst i1) (Inst i2) = Inst $ M.mapWithKey f i1
  where
    f relname r = r {tups = tups r `Set.union` maybe Set.empty tups (M.lookup relname i2)}

iEmpty :: Inst
iEmpty = Inst M.empty

---------
-- MISC --
---------
vToSet :: Ord a => Vector a -> Set a
vToSet = Set.fromList . V.toList

varToColName :: Val -> Text
varToColName x = T.pack $ "x" <> show x
