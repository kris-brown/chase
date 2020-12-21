import Data.Maybe (fromJust)
import Data.Set as Set
import Data.Vector as V
import Dep
import Rel
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

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

-- Triangle with bidirectional edges
triangleBiDir :: Relation
triangleBiDir = triangle {tups = (\x -> Set.union x (Set.map V.reverse x)) (tups triangle)}

iTriangleN :: Inst
iTriangleN =
  Inst $
    Set.fromList
      [ triangle11,
        relFromLists "N" ["col1"] []
      ]

iAB :: Inst
iAB = Inst $ Set.fromList [a, b]

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

egdBidirectional :: EGD
egdBidirectional = mkEGD $ qFromAtoms [edge, edge']

egdEdge :: EGD
egdEdge = mkEGD edgeQ

dirToBiDir :: TGD
dirToBiDir = TGD edgeQ (Set.fromList [edge'])

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ testGroup
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
                (run (qFromAtoms [q1]) a),
            testCase "queryBasic2" $
              assertEqual
                ""
                (relFromLists "" ["x4"] [["x"], ["y"]])
                (run (qFromAtoms [q2]) b),
            testCase "queryIdentity" $
              assertEqual
                "Running egdEdge on any relation that is just a set of edges (two columns) should return identity"
                (tups triangle)
                (tups (run (egd egdEdge) triangle)),
            testCase "queryEmpty1" $
              assertEqual
                ""
                Set.empty
                (tups (run (egd egdBidirectional) triangle)),
            testCase "queryEmpty2" $
              assertEqual
                ""
                Set.empty
                (tups (run (qFromAtoms [selfEdge]) triangle)),
            testCase "querySelfLoop" $
              assertEqual
                ""
                (fromLists [["1", "1"]])
                (tups $ run (qFromAtoms [selfEdge]) triangle11),
            testCase "fireTriangleEGD" $
              assertEqual
                "In theory this could be 1 2 or 3"
                (fromLists [["3", "3"]])
                (tups $ fromJust $ fireEGD triangle egdEdge),
            testCase "fireTriangleTGD" $
              assertEqual
                ""
                triangleBiDir
                (fst $ fireTGD dirToBiDir triangle 0)
          ]
      ]

{- TURN THIS INTO A PROPERTY TEST
              testCase "queryIdentity" $
              assertEqual
                "Running egdEdge on any relation that is just a set of edges (two columns) should return identity"
                generate X --(any 2-col relation)
                (tups X)
                (tups (run (egd egdEdge) X)),

                -}