import Data.Maybe (fromJust)
import Data.Set as Set
import Data.Validity (Validity (..), check)
import Data.Vector as V
import Data.Map as M
import Dep (Query (..),
            TGD (..),
            EGD (..),
            Atom (..),
            run,
            fireTGD,
            fireEGD,
            atomFromList,
            mkEGD,
            qFromAtoms,
            qRename)
import GHC.Generics (Generic)
import Rel
  ( Inst (..),
    Relation (..),
    fromCSV,
    fromLists,
    fromRels,
    genRel,
    relFromLists,
  )
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (Spec, hspec, it, shouldBe)
import Test.QuickCheck (genericShrink)
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Validity (GenUnchecked (..), GenValid (..), forAllValid, genValidGeneratesValid)

--------------
-- Examples --
--------------

a_ :: Relation
a_ =
  relFromLists
    "A"
    ["col1", "col2", "col3"]
    [ ["1", "x", "2"],
      ["c", "d", "3"]
    ]

a :: Relation
a = unsafePerformIO $ fromCSV "A"

b :: Relation
b = unsafePerformIO $ fromCSV "B"

triangle :: Relation
triangle = unsafePerformIO $ fromCSV "Triangle"

triangle11 :: Relation
triangle11 = unsafePerformIO $ fromCSV "TriangleLoop"

{-# NOINLINE a #-}

{-# NOINLINE b #-}

{-# NOINLINE triangle #-}

{-# NOINLINE triangle11 #-}

iTriangle :: Inst
iTriangle = fromRels [triangle]

-- Triangle with bidirectional edges
triangleBiDir :: Relation
triangleBiDir = triangle {tups = (\x -> Set.union x (Set.map V.reverse x)) (tups triangle)}

iTriangleN :: Inst
iTriangleN =
  fromRels
    [ triangle11,
      relFromLists "N" ["col1"] []
    ]

iAB :: Inst
iAB = fromRels [a, b]

q1 :: Atom
q1 = atomFromList "A" ["c", "4", "5"]

q2 :: Atom
q2 = atomFromList "B" ["x", "4"]

{-
q3 = Set.fromList [q1, q2]
-}
edge :: Atom
edge = atomFromList "Triangle" ["0", "1"]

edge' :: Atom
edge' = atomFromList "Triangle" ["1", "0"]

edgeQ :: Query
edgeQ = qFromAtoms [edge]

selfEdge :: Atom
selfEdge = atomFromList "TriangleLoop" ["0", "0"]

selfEdgeTriangle :: Query
selfEdgeTriangle = qRename (qFromAtoms [selfEdge]) (M.fromList [("TriangleLoop", "Triangle")])

egdBidirectional :: EGD
egdBidirectional = mkEGD $ qFromAtoms [edge, edge']

egdEdge :: EGD
egdEdge = mkEGD edgeQ

dirToBiDir :: TGD
dirToBiDir = TGD edgeQ (Set.fromList [edge'])

newtype TwoColRel = TwoColRel Relation deriving (Show, Eq, Generic)

instance Validity TwoColRel where
  validate (TwoColRel r@(Relation _ ra _)) =
    mconcat
      [ check (V.length ra == 2) "TwoColRel has 2 attrs",
        validate r
      ]

instance GenUnchecked TwoColRel where
  genUnchecked = TwoColRel <$> genValid
  shrinkUnchecked = genericShrink

instance GenValid TwoColRel where
  genValid = TwoColRel <$> genRel 2

unitTests :: TestTree
unitTests =
  testGroup
    "UnitTests"
    [ testCase "fromCSV" $ assertEqual "" a_ a,
      testCase "join" $
        assertEqual
          ""
          (a <> b)
          ( relFromLists
              ""
              ["col1", "col3", "col4"]
              [ ["1", "2", "x"],
                ["1", "2", "y"]
              ]
          ),
      testCase "queryBasic" $
        assertEqual
          ""
          (relFromLists "" ["x4", "x5"] [["d", "3"]])
          (run (qFromAtoms [q1]) (fromRels [a])),
      testCase "queryBasic2" $
        assertEqual
          ""
          (relFromLists "" ["x4"] [["x"], ["y"]])
          (run (qFromAtoms [q2]) (fromRels [b])),
      testCase "queryIdentity" $
        assertEqual
          "Running egdEdge on any relation that is just a set of edges (two columns) should return identity"
          (tups triangle)
          (tups (run (egd egdEdge) iTriangle)),
      testCase "queryEmpty1" $
        assertEqual
          ""
          Set.empty
          (tups (run (egd egdBidirectional) iTriangle)),
      testCase "queryEmpty2" $
        assertEqual
          ""
          Set.empty
          (tups (run selfEdgeTriangle iTriangle)),
      testCase "querySelfLoop" $
        assertEqual
          ""
          (fromLists [["1"]])
          (tups $ run (qFromAtoms [selfEdge]) (fromRels [triangle11])),
      testCase "fireTriangleEGD" $
        assertEqual
          "In theory this could be 1 2 or 3"
          (fromLists [["3", "3"]])
          (tups $ rels (fromJust $ fireEGD iTriangle egdEdge) M.! "Triangle"),
      testCase "fireTriangleTGD" $
        assertEqual
          ""
          (fromRels [triangleBiDir])
          (fst $ fireTGD dirToBiDir iTriangle 0)
    ]

propertyTests :: Spec
propertyTests = do
  it "egdEdge query is identity on TwoColRels" $ do
    forAllValid $ \(TwoColRel r) ->
      tups (run (egd egdEdge) (fromRels [r{name="Triangle"}])) `shouldBe` tups r
  it "twoColGen is valid" (genValidGeneratesValid @TwoColRel)

main :: IO ()
main = do
  hspec propertyTests
  Test.Tasty.defaultMain unitTests
