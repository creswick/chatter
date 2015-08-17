module TestUtils
  ( genTestF2
  , genTest3
  , genTestF3
  , genTest2
  , genTest
  , genTestF
  , assertApproxEquals
  )
where


import Control.Monad (unless)
import Test.HUnit      ( (@=?), Assertion, assertFailure )
import qualified Test.Tasty.HUnit as TH
import Test.Tasty (TestTree)

-- | Not available until Base 4.7 or greater:
isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

testDescrLen :: Int
testDescrLen = 50

truncateDescr :: String -> String
truncateDescr str | length str > 20 = (take testDescrLen str) ++ "..."
                  | otherwise = str

testCase :: String -> TH.Assertion -> TestTree
testCase str = TH.testCase (truncateDescr str)

genTestF2 :: (Show a, Show b) => (a -> b -> Double) -> (String, a, b, Double) -> TestTree
genTestF2 fn (descr, in1, in2, oracle) =
    testCase (descr++" [input: "++ show in1++"," ++show in2++"]") assert
        where assert = assertApproxEquals "" 0.001 oracle $ fn in1 in2

genTest3 :: (Show a, Show b, Show c, Show d, Eq d)
         => (a -> b -> c -> d)
         -> (String, a, b, c, d)
         -> TestTree
genTest3 fn (descr, in1, in2, in3, oracle) =
    testCase (descr++" [input: "++ show in1++"," ++show in2++"," ++show in3++"]") assert
        where assert = oracle @=? fn in1 in2 in3

genTestF3 :: (Show a, Show b, Show c)
         => (a -> b -> c -> Double)
         -> (String, a, b, c, Double)
         -> TestTree
genTestF3 fn (descr, in1, in2, in3, oracle) =
    testCase (descr++" [input: "++show in1++"," ++show in2++"," ++show in3++"]") assert
        where assert = assertApproxEquals "" 0.001 oracle $ fn in1 in2 in3

genTest2 :: (Show a, Show b, Show c, Eq c) => (a -> b -> c) -> (String, a, b, c) -> TestTree
genTest2 fn (descr, in1, in2, oracle) =
    testCase (descr++" [input: "++show in1++"," ++show in2++"]") assert
        where assert = oracle @=? fn in1 in2

genTest :: (Show a, Show b, Eq b) => (a -> b) -> (String, a, b) -> TestTree
genTest fn (descr, input, oracle) =
    testCase (descr++" [input: "++show input++"]") assert
        where assert = oracle @=? fn input

genTestF :: Show a => (a -> Double) -> (String, a, Double) -> TestTree
genTestF fn (descr, input, oracle) =
    testCase (descr++" [input: "++show input++"]") assert
        where assert = assertApproxEquals "" 0.001 oracle $ fn input

assertApproxEquals :: String  -- ^ The message prefix
                  -> Double  -- ^ The maximum difference between expected and actual
                  -> Double  -- ^ The expected value
                  -> Double  -- ^ The actual value
                  -> Assertion
assertApproxEquals preface delta expected actual =
  unless (abs (expected - actual) < delta) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n but got: " ++ show actual
