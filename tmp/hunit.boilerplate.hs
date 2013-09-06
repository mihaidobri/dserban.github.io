import Test.HUnit

-- Domain-specific functions

a :: Integer
a = 1

b :: Integer
b = 1

c :: Integer
c = 2

-- Test cases

test1 :: Test
test1 = TestCase $ assertEqual "" a b

test2 :: Test
test2 = TestCase $ assertEqual "" a c

test3 :: Test
test3 = TestCase $ assertBool "" ( a == b )

tests :: Test
tests = TestList [ TestLabel "First test"  test1
                 , TestLabel "Second test" test2
                 , TestLabel "Third test"  test3
                 ]

-- Test runner

main :: IO ()
main = runTestTT tests >>= print
