module Rel
  ( Var,
    Val (..),
    Tup,
    Relation (..),
    RelName,
    Inst (..),
    fromLists,
    renameAttrs,
    relFromLists,
    fromString,
    fromCSV,
    join,
    joins,
    varToColName,
    tupFromList,
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
import Data.Vector as V
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getHomeDirectory)
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
  deriving (Eq, Ord)

-- Instances are sets of relations
newtype Inst = Inst {rels :: Set Relation}

------------------------------------------------------
--import Control.Lens ()
--makeLenses ''Val
--makeLenses ''Atom

------------------------------------------------------------
---------------
-- Instances --
---------------
instance Validity Val where validate = trivialValidation

instance Validity Relation where
  validate r =
    mconcat
      [ check (val' (tups r)) "All tuples have the same length as the # of attrs.",
        check ((==) n (Set.size $ Set.fromList ats)) "All attrs should have unique names"
      ]
    where
      ats = V.toList $ attrs r
      n = P.length ats
      val' ts = P.and $ Set.map ((== n) . V.length) ts

instance Validity Inst where
  validate (Inst rs) = check ((==) (P.length names) (P.length $ L.nub names)) "All relations have unique names"
    where
      names = Set.toList $ Set.map name rs

-- Join is an associative operation
instance Semigroup Relation where
  a <> b = join False a b

instance Show Val where
  show (V i) = show i
  show (C x) = T.unpack x

-- View the relation as a table
instance Show Relation where
  show (Relation name attrs tups) =
    P.concat
      [ T.unpack name,
        "\n",
        Box.render $ table $ (T.unpack <$> V.toList attrs) : P.map (P.map show . V.toList) (Set.toList tups)
      ]

------------------------------------------------------------
------------------------
-- Printing functions --
------------------------
pad :: Int -> String -> String
pad width x = x <> P.replicate k ' '
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
    vsep = vcat Box.left $ Box.char <$> ("+" <> P.concat (L.replicate nrows "|+"))

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
relFromLists name attr tups = Relation (T.pack name) (V.fromList $ P.map T.pack attr) (fromLists tups)

-- Friendly constructor for instances
mkInst :: [(String, [String], [[String]])] -> Inst
mkInst = Inst . Set.fromList . P.map (uncurry3 relFromLists)

-- Parse a CSV file. Path is default as interpreted as the name
-- of a file in the /data/ folder, treated as absolute if that's
-- not successful
fromCSV :: FilePath -> IO Relation
fromCSV filepath = do
  dfault <- (\home -> P.concat [home, "/chase/data/", filepath, ".csv"]) <$> getHomeDirectory
  check <- doesFileExist dfault
  let filepath' = if check then dfault else filepath
  parseResult <- parseCSVFromFile filepath'
  return $ case parseResult of
    Left _ -> error ("Can't find/parse " <> filepath)
    Right (header : rows) -> relFromLists filepath header rows

------------------------------------------------------------
-------------------------------
-- Basic relation operations --
-------------------------------
--Canonical empty relation
rEmpty :: Relation
rEmpty = Relation "" V.empty Set.empty

-- Rename attrs with a map
renameAttrs :: Relation -> Map AttrName AttrName -> Relation
renameAttrs rel attrMap = rel {attrs = updated}
  where
    updated = V.map (\attr -> M.findWithDefault attr attr attrMap) $ attrs rel

------------------------------------------------------------
-- JOIN --
----------
-- The merge step can be made faster with V.backpermute(unsafe)
join :: Bool -> Relation -> Relation -> Relation
join saveOverlap (Relation _ nAttr nTup) (Relation _ mAttr mTup) = Relation "" (V.fromList (overlapAttr P.++ resattr)) (Set.fromList tup)
  where
    allattr = Set.fromList (V.toList nAttr <> V.toList mAttr)
    overlap = Set.intersection (vToSet nAttr) (vToSet mAttr)
    overlapAttr = if not saveOverlap then [] else Set.toList overlap
    resattr = Set.toList $ Set.difference allattr overlap
    getinds x = P.map (\i -> MB.fromJust $ L.elemIndex i $ V.toList x) $ Set.toList overlap
    inds = P.zip (getinds nAttr) (getinds mAttr)
    attrOnly x = Set.toList $ Set.difference (vToSet x) overlap
    newinds = (if saveOverlap then getinds nAttr else []) <> [MB.fromJust $ L.elemIndex a $ V.toList nAttr | a <- attrOnly nAttr]
    mNewInds = [MB.fromJust $ L.elemIndex a $ V.toList mAttr | a <- attrOnly mAttr]
    merge x y = V.fromList $ [x V.! i | i <- newinds] <> [y V.! i | i <- mNewInds]
    tup = [merge nt mt | nt <- Set.toList nTup, mt <- Set.toList mTup, P.and [nt V.! i == mt V.! j | (i, j) <- inds]]

joins :: [Relation] -> Bool -> Relation
joins [] _ = rEmpty
joins [x] _ = x
joins (h : t) sO = join sO h $ joins t sO

------------------------------------------------------------
---------
-- MISC --
---------
vToSet :: Ord a => Vector a -> Set a
vToSet = Set.fromList . V.toList

varToColName :: Val -> Text
varToColName x = T.pack $ "x" <> show x
