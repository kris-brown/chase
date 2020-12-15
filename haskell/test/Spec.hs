import Data.Map as M
import Data.Maybe (fromJust)
import Data.Set as Set
import Data.Vector as V
import Lib
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

--------------
-- Examples --
--------------
a_ =
  relFromLists
    "A"
    ["col1", "col2", "col3"]
    [ ["1", "x", "2"],
      ["c", "d", "3"]
    ]

a = unsafePerformIO $ fromCSV "A"

b = unsafePerformIO $ fromCSV "B"

triangle = unsafePerformIO $ fromCSV "Triangle"

triangle11 = unsafePerformIO $ fromCSV "TriangleLoop"

{-# NOINLINE a #-}

{-# NOINLINE b #-}

{-# NOINLINE triangle #-}

{-# NOINLINE triangle11 #-}

iTriangleN =
  Set.fromList
    [ triangle11,
      relFromLists "N" ["col1"] []
    ]

iAB = Set.fromList [a, b]

q1 = atomFromList "A" ["c", "4", "5"]

q2 = atomFromList "B" ["x", "4"]

q3 = Set.fromList [q1, q2]

edge = atomFromList "Triangle" ["0", "1"]

edge' = atomFromList "Triangle" ["1", "0"]

selfEdge = atomFromList "TriangleLoop" ["0", "0"]

egdBidirectional = EGD $ qFromAtoms [edge, edge']

egdEdge = EGD $ qFromAtoms [edge]

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
            testCase "queryIdentity" $
              assertEqual
                ""
                (tups triangle)
                (tups (run (egd egdEdge) triangle)),
            testCase "queryEmpty" $
              assertEqual
                ""
                Set.empty
                (tups (run (egd egdBidirectional) triangle)),
            testCase "query" $ assertEqual "" (fromLists [["1", "1"]]) (tups $ run (qFromAtoms [selfEdge]) triangle11),
            testCase "fireTriangleCollapse" $ assertEqual "" (fromLists [["1", "1"]]) (tups $ fromJust $ fireEGD egdEdge triangle)
          ]
      ]
