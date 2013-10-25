{-# LANGUAGE OverloadedStrings #-}
module NLP.Similarity.VectorSimTests where

import Control.Monad (unless)
import Test.HUnit      ( (@=?), Assertion, assertBool, assertFailure )
import Test.QuickCheck ( Arbitrary(..), Property, (==>), elements )
import Test.QuickCheck.Property ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework ( buildTest, testGroup, Test, defaultMain )
import Test.Framework.Skip (skip)

import Data.Text (Text)
import qualified Data.Text as T

import NLP.Similarity.VectorSim
import NLP.Types (mkCorpus)

tests :: Test
tests = testGroup "Vector Sim"
        [ testGroup "Dot Products" $ map (genTestF2 dotProd)
          [ ("3-4-5", [1,3,-5], [4, -2, -1], 3)
          , ("identical", [1], [1], 1.0)
          , ("orthogonal", [1,0], [0,1], 0)
          ]
        , testGroup "cosines" $ map (genTestF2 cosVec)
          -- ("identical", [0], [0], NaN) -- can't determine the angle.
          [ ("identical", [1], [1], 1.0)
          , ("identical", [1,2], [1,2], 1.0)
          , ("orthogonal", [1,0], [0,1], 0)
          ]
        , testGroup "Magnitude tests" $ map (genTest magnitude)
          [ ("3-4-5", [3,4], 5)
          , ("empty", [], 0)
          , ("single", [1], 1)
          ]
        , testGroup "tf tests" $ map (genTest2 tf)
          [ ("", "test", ["test"], 1)
          , ("", "a", ["a", "test"], 1)
          , ("", "a", ["test"], 0)
          ]
        , testGroup "idf tests" $ map (genTestF2 idf)
          [ ("", "test", mkCorpus [["test"]], log(1/2))
          , ("", "a", mkCorpus [["test"]], log(1))
          , ("", "a", mkCorpus [["a", "test"],["test"]], log(2/2))
          ]
        , testGroup "tf_idf tests" $ map (genTestF3 tf_idf)
          [ ("", "test", ["test"], mkCorpus [["test"]], log(2/3))
          , ("", "a", ["a", "test"], mkCorpus [["some"], ["test"]], 0.40546)
          , ("", "foo", ["foo"], mkCorpus [["test"]], 0)
          , ("", "foo", ["foo"], mkCorpus [["foo"],["test"]], 0)
          , ("", "bar", ["foo"], mkCorpus [["test"]], 0)
          , ("", "bar", ["foo"], mkCorpus [["foo"],["test"]], 0)
          ]
        , testGroup "Similarity tests, trivial corpus" $
            map (genTest2 $ sim $ mkCorpus [["test"]])
                    [ ("same doc", "test", "test", 1)
                      -- This next test is invalid becausse the
                      -- initial smoothing causes funny results (this
                      -- should not be 1.0, and it /is not/ when the
                      -- corpus is bigger.)
                    , ("one off", "a test", "the test", 1.0)
                    , ("No match", "foo", "bar", 0.0)
                    ]
        , testGroup "Similarity tests, minor corpus" $
            map (genTestF2 $ sim $ mkCorpus [ ["a", "sample"]
                                            , ["the", "test"]
                                            , ["big", "example"]
                                            , ["more", "terms"]])
                    [ ("same doc", "test", "test", 1)
                    , ("one off", "a test", "the test", 0.5)
                    , ("No match", "foo", "bar", 0.0)
                    ]
        , testProperty "idf /= NaN" prop_idfIsANum
        , testProperty "tf_idf /= NaN" prop_tf_idfIsANum
        , testProperty "dotProd /= NaN" prop_dotProd_isANum
        , testProperty "cosVec /= NaN" prop_cosVec_isANum
        , testProperty "magnitude /= NaN" prop_magnitude_isANum
        , testProperty "similarity /= NaN" prop_similarity_isANum
        ]

genTestF2 :: (Show a, Show b) => (a -> b -> Double) -> (String, a, b, Double) -> Test
genTestF2 fn (descr, in1, in2, oracle) =
    testCase (descr++" [input: "++show in1++"," ++show in2++"]") assert
        where assert = assertApproxEquals "" 0.001 oracle $ fn in1 in2

genTest3 :: (Show a, Show b, Show c, Show d, Eq d)
         => (a -> b -> c -> d)
         -> (String, a, b, c, d)
         -> Test
genTest3 fn (descr, in1, in2, in3, oracle) =
    testCase (descr++" [input: "++show in1++"," ++show in2++"," ++show in3++"]") assert
        where assert = oracle @=? fn in1 in2 in3


genTestF3 :: (Show a, Show b, Show c)
         => (a -> b -> c -> Double)
         -> (String, a, b, c, Double)
         -> Test
genTestF3 fn (descr, in1, in2, in3, oracle) =
    testCase (descr++" [input: "++show in1++"," ++show in2++"," ++show in3++"]") assert
        where assert = assertApproxEquals "" 0.001 oracle $ fn in1 in2 in3

genTest2 :: (Show a, Show b, Show c, Eq c) => (a -> b -> c) -> (String, a, b, c) -> Test
genTest2 fn (descr, in1, in2, oracle) =
    testCase (descr++" [input: "++show in1++"," ++show in2++"]") assert
        where assert = oracle @=? fn in1 in2

genTest :: (Show a, Show b, Eq b) => (a -> b) -> (String, a, b) -> Test
genTest fn (descr, input, oracle) =
    testCase (descr++" [input: "++show input++"]") assert
        where assert = oracle @=? fn input

prop_idfIsANum :: String -> [[String]] -> Bool
prop_idfIsANum term docs = not (isNaN (idf termTxt $ mkCorpus docsTxt))
  where
    termTxt = T.pack term
    docsTxt = map (map T.pack) docs

prop_tf_idfIsANum :: String -> [String] -> [[String]] -> Bool
prop_tf_idfIsANum term doc docs = not $ isNaN $ tf_idf termTxt docTxt $ mkCorpus docsTxt
  where
    termTxt = T.pack term
    docTxt = map T.pack doc
    docsTxt = map (map T.pack) docs

prop_dotProd_isANum :: [Double] -> [Double] -> Bool
prop_dotProd_isANum xs ys = not $ isNaN $ dotProd xs ys

prop_cosVec_isANum :: [Double] -> [Double] -> Property
prop_cosVec_isANum xs ys = (xs /= []) && (ys /= []) ==>
  not $ isNaN $ cosVec xs ys

prop_magnitude_isANum :: [Double] -> Bool
prop_magnitude_isANum xs = not $ isNaN $ magnitude xs

prop_similarity_isANum :: [[String]] -> [String] -> [String] -> Property
prop_similarity_isANum strCorp d1 d2 = strCorp /= [] &&
                                       (concat d1 /= []) &&
                                       (concat d2 /= [])==> let
  corpus = mkCorpus $ map (map T.pack) strCorp
  doc1 = map T.pack d1
  doc2 = map T.pack d2
  in not $ isNaN $ similarity corpus doc1 doc2

assertApproxEquals :: String  -- ^ The message prefix
                  -> Double  -- ^ The maximum difference between expected and actual
                  -> Double  -- ^ The expected value
                  -> Double  -- ^ The actual value
                  -> Assertion
assertApproxEquals preface delta expected actual =
  unless (abs (expected - actual) < delta) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n but got: " ++ show actual