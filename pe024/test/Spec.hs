import Test.HUnit
import Lib

f expected n = TestLabel "" $ TestCase $ assertEqual "" expected actual
  where actual = factorials n

tests = TestList
  [ f [] (-1)
  , f [] 0
  , f [1] 1
  , f [2, 1, 1] 3
  , f [720, 120, 24, 6, 2, 1, 1] 7
  ]

main = runTestTT tests
