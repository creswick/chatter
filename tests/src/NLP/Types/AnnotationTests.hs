{-# LANGUAGE OverloadedStrings #-}
module NLP.Types.AnnotationTests where

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck (testProperty, Property, (==>), generate, QuickCheckTests(..), QuickCheckMaxSize(..))
import Test.HUnit      ( (@=?) )
import Test.Tasty.HUnit (testCase)
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers

import Data.List (group)
import Data.Serialize (decode, encode)
import Data.Text (Text)
import qualified Data.Text as T

import NLP.Types
import NLP.Types.Arbitrary
import NLP.Corpora.Parsing
import qualified NLP.Corpora.Conll as C
import qualified NLP.Corpora.Brown as B

tests :: TestTree
tests = testGroup "NLP.Types.Annotations"
        [ testGroup "prettyShow round-trip properties"
          [ testProperty "TaggedSentence: RawTag"
            (prop_taggedSentence_roundtrip_raw :: TaggedSentence RawTag -> Bool)
          , testProperty "TaggedSentence: Connl Tag"
            (prop_taggedSentence_roundtrip_connl :: TaggedSentence C.Tag -> Bool)
          , testProperty "TaggedSentence: Brown Tag"
            (prop_taggedSentence_roundtrip_brown :: TaggedSentence B.Tag -> Bool)
          ]
        , testGroup "prettyShow round-trip fixed tests" $
          map mkPrettyShowRTTest
          [ "the/DT dog/NN jumped/VB ./."
          ]
        , testGroup "Manual tests of TaggedSentence" $
          map mkReadPOSTest
          [ readPOSTest1 ]
        , testGroup "tsToPairs" $
          map mkTsToPairsTest
          [ ( "the/DT dog/NN jumped/VB ./."
            , [ (Token "the", RawTag "DT")
              , (Token "dog", RawTag "NN")
              , (Token "jumped", RawTag "VB")
              , (Token ".", RawTag ".")
              ])
          ]
        , testGroup "Direct tsToPairs" $
          map mkDirectTsToPairsTest
          [ ( "h"
            , let pld = TokenizedSentence {tokText = "h"
                                          , tokAnnotations = [Annotation {startIdx = Index 0
                                                                         , len = 1
                                                                         , value = Token "h"
                                                                         , payload = "h"}]}
              in TaggedSentence
                   { tagTokSentence = pld
                   , tagAnnotations =
                     [ Annotation { startIdx = Index 0
                                  , len = 1
                                  , value = B.PPO
                                  , payload = pld }
                     ]
                   }
            , [ (Token "h", B.PPO) ])
          ]
        , testGroup "ChunkedSentence helpers"
          [ localOption (QuickCheckMaxSize 15) $
              localOption (QuickCheckTests 15) $
                testProperty "To/FromChunkedSentence are duals of eachother"
                  prop_toFromChunkedSentence
          , testProperty "mkTokenizedSent is the right length"
            prop_mkTokSentLength
          ]
        ]

prop_mkTokSentLength :: Positive Int -> Property
prop_mkTokSentLength (Positive x) = monadicIO $ do
  tsent <- run $ generate $ mkTokenizedSent x
  assert (x == (length $ tokAnnotations tsent))

prop_toFromChunkedSentence :: ChunkedSentence B.Tag B.Chunk -> Property
prop_toFromChunkedSentence expected =
  length (group $ map value $ chunkAnnotations expected) == (length $ map value $ chunkAnnotations expected) ==>
  let actual = (uncurry toChunkedSentence) $ fromChunkedSentence expected
  in actual == expected

mkDirectTsToPairsTest :: POS pos => (Text, TaggedSentence pos, [(Token, pos)]) -> TestTree
mkDirectTsToPairsTest (txt, sent, expected) = testCase (T.unpack txt) $
   expected @=? tsToPairs sent

mkTsToPairsTest :: POS pos => (Text, [(Token, pos)]) -> TestTree
mkTsToPairsTest (txt, expected) = mkDirectTsToPairsTest (txt, readPOS txt, expected)

readPOSTest1 :: (Text, TaggedSentence RawTag)
readPOSTest1 =
  let readPOSTestTxt1 = "the/DT dog/NN"
                      -- 012   3456
      plainText = "the dog"
      tokSent = TokenizedSentence
                { tokText = plainText
                , tokAnnotations =
                  [ Annotation { startIdx = Index 0
                               , len = 3
                               , value = Token "the"
                               , payload = plainText
                               }
                  , Annotation { startIdx = Index 4
                               , len = 3
                               , value = Token "dog"
                               , payload = plainText
                               }
                  ]
                }
  in ( readPOSTestTxt1
     , TaggedSentence
       { tagTokSentence = tokSent
       , tagAnnotations =
         [ Annotation { startIdx = Index 0
                      , len = 1
                      , value = RawTag "DT"
                      , payload = tokSent
                      }
         , Annotation { startIdx = Index 1
                      , len = 1
                      , value = RawTag "NN"
                      , payload = tokSent
                      }
         ]
       }
     )

mkReadPOSTest :: (Text, TaggedSentence RawTag) -> TestTree
mkReadPOSTest (input, expected) = testCase (T.unpack input) $ do
  let actual = readPOS input
  expected @=? actual

-- | readPOS is only actually expected to work with "well-formed"
-- training corpora, which already have tags, so we first serialize
-- the randomly-generated inputs, saving the result as the
-- "ground-truth".
-- Then we parse / serialize, and expect that "ground-truth" back again.
prop_taggedSentence_roundtrip_raw :: TaggedSentence RawTag -> Bool
prop_taggedSentence_roundtrip_raw ts = let expected = normalizeWS $ prettyShow ts

                                           normalizeWS :: Text -> Text
                                           normalizeWS = T.unwords . T.words

                                           structured :: TaggedSentence RawTag
                                           structured = readPOS expected
                                           actual = prettyShow structured

                                        in expected == actual

prop_taggedSentence_roundtrip_brown :: TaggedSentence B.Tag -> Bool
prop_taggedSentence_roundtrip_brown ts = let expected = normalizeWS $ prettyShow ts

                                             normalizeWS :: Text -> Text
                                             normalizeWS = T.unwords . T.words

                                             structured :: TaggedSentence B.Tag
                                             structured = readPOS expected
                                             actual = prettyShow structured
                                        in expected == actual

prop_taggedSentence_roundtrip_connl :: TaggedSentence C.Tag -> Bool
prop_taggedSentence_roundtrip_connl ts = let expected = normalizeWS $ prettyShow ts

                                             normalizeWS :: Text -> Text
                                             normalizeWS = T.unwords . T.words

                                             structured :: TaggedSentence C.Tag
                                             structured = readPOS expected
                                             actual = prettyShow structured

                                         in expected == actual

prop_corpusSerialize :: Corpus -> Bool
prop_corpusSerialize c = case (decode . encode) c of
                           Right c' -> c == c'
                           Left _ -> False

mkPrettyShowRTTest :: Text -> TestTree
mkPrettyShowRTTest txt = testCase (T.unpack txt) $ do
  let taggedSent :: TaggedSentence RawTag
      taggedSent = readPOS txt
  txt @=? prettyShow taggedSent
