{-# LANGUAGE OverloadedStrings #-}
module NLP.Types.AnnotationTests where

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit      ( (@=?) )
import Test.Tasty.HUnit (testCase)

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
            (prop_taggedSentence_roundtrip :: TaggedSentence RawTag -> Bool)
          , testProperty "TaggedSentence: Connl Tag"
            (prop_taggedSentence_roundtrip :: TaggedSentence C.Tag -> Bool)
          , testProperty "TaggedSentence: Brown Tag"
            (prop_taggedSentence_roundtrip :: TaggedSentence B.Tag -> Bool)
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
            , TaggedSentence
              { tagTokSentence =
                  TokenizedSentence {tokText = "h"
                                    , tokAnnotations = [Annotation {startIdx = Index 0
                                                                   , len = 1
                                                                   , value = Token "h"
                                                                   , payload = "h"}]}
              , tagAnnotations =
                [ Annotation { startIdx = Index 0
                             , len = 1
                             , value = B.PPO
                             , payload =
                              TokenizedSentence {tokText = "h"
                                                , tokAnnotations = [Annotation {startIdx = Index 0
                                                                               , len = 1
                                                                               , value = Token "h"
                                                                               , payload = "h"}]}}]}
            , [ (Token "h", B.PPO)
              ])
          ]
        ]

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

prop_taggedSentence_roundtrip :: POS pos => TaggedSentence pos -> Bool
prop_taggedSentence_roundtrip ts = let actual = readPOS (prettyShow ts)
                                   in ts == actual

prop_corpusSerialize :: Corpus -> Bool
prop_corpusSerialize c = case (decode . encode) c of
                           Right c' -> c == c'
                           Left _ -> False

mkPrettyShowRTTest :: Text -> TestTree
mkPrettyShowRTTest txt = testCase (T.unpack txt) $ do
  let taggedSent :: TaggedSentence RawTag
      taggedSent = readPOS txt
  txt @=? prettyShow taggedSent
