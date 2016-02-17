{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module NLP.Types.IOBTests where

import Test.HUnit      ( (@=?), Assertion )

import Test.Tasty ( testGroup, TestTree )
import Test.QuickCheck (Arbitrary(..), elements, (==>), Property)
import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase, assertFailure)

import Data.Char (isSpace)
import Data.Either (rights)
import qualified Data.Text as T
import Data.Text (Text)

import qualified NLP.Corpora.Conll as C
import NLP.Types
import NLP.Types.IOB
import NLP.Types.Arbitrary

tests :: TestTree
tests = testGroup "NLP.Types.IOB"
        [ testGroup "getSentences" $ map mkGetSentencesTest
          [ ("One sentence", "This\nis\na\ntest\n."
            , ["This\nis\na\ntest\n."])
          , ("No text", "", [""])
          , ("Just whitespace", "  \n  ", ["  \n  "])
          , ("Two sentences (a)", "He\njumped\n.\n\nShe\njumped\n."
            , [ "He\njumped\n.\n"
              , "She\njumped\n."])
          , ("Two sentences (b)", T.unlines ["He", "jumped", "."
                                            , ""
                                            , "She", "jumped", "."]
            , [ "He\njumped\n.\n"
              , "She\njumped\n.\n"]) -- TODO why is this last \n needed?
          ]
        , testCase "IOB to Chunked Sentence" (test_parseIOB2cs iob2csExample)
        , testCase "IOB parse Annotations" (test_parseIOBAnns iobExampleAnns)
        , testCase "IOB parse" (test_parseIOB iobExample)
--        , testGroup "Parse Annotations to IOB" $ map test_parseAnnsToIOB parseAnnsToIOBTests
        ]

stripTok :: Token -> Token
stripTok (Token t) = Token $ T.strip t

test_parseIOB2cs :: (Text, [Text]) -> Assertion
test_parseIOB2cs (txt, oracle) = do
  let parsed :: Either Error [ChunkedSentence C.Tag C.Chunk]
      parsed = parseToChunkedSentences txt
  case parsed of
    Right iobs -> oracle @=? map prettyShow iobs
    Left   err -> assertFailure $ T.unpack err

iob2csExample :: (Text, [Text])
iob2csExample = (T.unlines [ "Confidence NN B-NP "
                        , "in IN B-PP"
                        , "the DT B-NP"
                        , "pound NN I-NP"
                        , ". . O"
                        , ""
                        , "is VBZ B-VP"
                        , "widely RB I-VP"
                        , "expected VBN I-VP"
                        , "to TO I-VP"
                        , "take VB I-VP"
                        , "another DT B-NP" ]
             , [ "[NP Confidence/NN] [PP in/IN] [NP the/DT pound/NN] ./."
               , "[VP is/VBZ widely/RB expected/VBN to/TO take/VB] [NP another]"
               ]
             )

-- test_parseAnnsToIOB :: (Int, Annotation Text Token, IOBTaggedSentence C.Tag) -> TestTree
-- test_parseAnnsToIOB (idx, input, oracle) = testCase ("Parse Anns to IOB: "++show idx) $
--   (Right oracle @=? parseIOB input)

-- parseAnnsToIOBTests :: [(Int, Annotation Text Token, IOBTaggedSentence C.Tag)]
-- parseAnnsToIOBTests =
--   let txt = T.unlines [ "Confidence NN B-NP "
--                       , "in IN B-PP"
--                       , "the DT B-NP"
--                       , "pound NN I-NP"
--                       , ". . O"
--                       , ""
--                       , "is VBZ B-VP"
--                       , "widely RB I-VP"
--                       , "expected VBN I-VP"
--                       , "to TO I-VP"
--                       , "take VB I-VP"
--                       , "another DT B-NP"
--                       ]
--   in [ ( 0
--        , Annotation { startIdx = Index 0
--                     , len = 63
--                     , value = Token "Confidence NN B-NP \nin IN B-PP\nthe DT B-NP\npound NN I-NP\n. . O\n"
--                     , payload = txt }
--        , IOBTaggedSentence { iobTagSentence =
--                                TaggedSentence { tagTokSentence = TokenizedSentence { tokText = "Confidence NN B-NP \nin IN B-PP\nthe DT B-NP\npound NN I-NP\n. . O\n"
--                                                                                    , tokAnnotations = [ Annotation { startIdx = Index 0
--                                                                                                                    , len = 10
--                                                                                                                    , value = Token "Confidence"
--                                                                                                                    , payload = "Confidence NN B-NP \nin IN B-PP\nthe DT B-NP\npound NN I-NP\n. . O\n" }
--                                                                                                       , Annotation { startIdx = Index 20
--                                                                                                                    , len = 2
--                                                                                                                    , value = Token "in"
--                                                                                                                    , payload = "Confidence NN B-NP \nin IN B-PP\nthe DT B-NP\npound NN I-NP\n. . O\n"}
--                                                                                                       ]
--                                                                                    }
--                                               , tagAnnotations = []
--                                               }
--                            , iobAnnotations = []
--                            }
--        )
--      , ( 1
--        , Annotation { startIdx = Index 64
--                     , len = 85
--                     , value = Token "is VBZ B-VP\nwidely RB I-VP\nexpected VBN I-VP\nto TO I-VP\ntake VB I-VP\nanother DT B-NP\n"
--                     , payload = txt }
--        , IOBTaggedSentence { iobTagSentence =
--                                TaggedSentence { tagTokSentence =
--                                                   TokenizedSentence { tokText = "is VBZ B-VP\nwidely RB I-VP\nexpected VBN I-VP\nto TO I-VP\ntake VB I-VP\nanother DT B-NP\n"
--                                                                     , tokAnnotations = []
--                                                                     }
--                                               , tagAnnotations = []
--                                               }
--                            , iobAnnotations = []
--                            }
--        )
--      ]

-- | Test that we can split IOB text into sentences (abusing the
-- Annotation Text Token type to store sentences)
test_parseIOBAnns :: (Text, [Annotation Text Token]) -> Assertion
test_parseIOBAnns (txt, oracle) = oracle @=? parseIOBSentences txt

iobExampleAnns :: (Text, [Annotation Text Token])
iobExampleAnns = let txt = T.unlines [ "Confidence NN B-NP "
                                     , "in IN B-PP"
                                     , "the DT B-NP"
                                     , "pound NN I-NP"
                                     , ". . O"
                                     , ""
                                     , "is VBZ B-VP"
                                     , "widely RB I-VP"
                                     , "expected VBN I-VP"
                                     , "to TO I-VP"
                                     , "take VB I-VP"
                                     , "another DT B-NP"
                                     ]
                 in (txt, [ Annotation { startIdx = Index 0
                                       , len = 63
                                       , value = Token "Confidence NN B-NP \nin IN B-PP\nthe DT B-NP\npound NN I-NP\n. . O\n"
                                       , payload = txt }
                          , Annotation { startIdx = Index 64
                                       , len = 85
                                       , value = Token "is VBZ B-VP\nwidely RB I-VP\nexpected VBN I-VP\nto TO I-VP\ntake VB I-VP\nanother DT B-NP\n"
                                       , payload = txt }
                          ]
                    )

test_parseIOB :: (Text, [IOBTaggedSentence C.Tag]) -> Assertion
test_parseIOB (txt, oracle) = do
  let parsed :: Either Error [IOBTaggedSentence C.Tag]
      parsed = parse txt
  case parsed of
    Right iobs -> oracle @=? iobs
    Left   err -> assertFailure $ T.unpack err

iobExample :: (Text, [IOBTaggedSentence C.Tag])
iobExample = (T.unlines [ "Confidence NN B-NP "
                        , "in IN B-PP"
                        , "the DT B-NP"
                        , "pound NN I-NP"
                        , ". . O"
                        , ""
                        , "is VBZ B-VP"
                        , "widely RB I-VP"
                        , "expected VBN I-VP"
                        , "to TO I-VP"
                        , "take VB I-VP"
                        , "another DT B-NP" ]
             , [
               ]
             )


mkGetSentencesTest :: (String, Text, [Text]) -> TestTree
mkGetSentencesTest (name, input, oracle) =
  testCase name (oracle @=? (map getText $ parseIOBSentences input))
