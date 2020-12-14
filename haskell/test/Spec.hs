import Data.Map as M
import Data.Set as Set
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

selfEdge = atomFromList "TriangleLoop" ["0", "0"]

egdBidirectional = EGD $ Set.fromList [edge, atomFromList "Triangle" ["1", "0"]]

egdEdge = EGD $ Set.fromList [edge]

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
            testCase "query" $ assertEqual "" triangle (rename (run egdEdge triangle) $ M.fromList [("x0", "src", "x1", "tar")])
          ]
      ]
