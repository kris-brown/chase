import Test.HUnit
import Lib
import Data.Map ((!))

inst_example = mkInst [("A",[[(C "a"), (C "b")],
                             [(V 0),   (V 1)],
                             [(V 2),   (C "c")]])]

test_project = TestCase (assertEqual ""
    (project (inst_example ! "A") 0)
    [(C "a"), (V 0), (V 2)])

test_query = TestCase (assertEqual "" (query [(C "a"), (C "b")] ) ())

test1 = TestCase (assertEqual "xxxx" (1 + 1 + 1) (3))

tests = TestList [TestLabel "test1" test1, TestLabel "test_project" test_project]

main :: IO ()
main = do _ <- runTestTT tests
          return ()

