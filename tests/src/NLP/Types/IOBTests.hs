{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module NLP.Types.IOBTests where

import Test.HUnit      ( (@=?), Assertion )

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit (testCase, assertFailure)

import Data.Either (rights)
import qualified Data.Text as T
import Data.Text (Text)

import qualified NLP.Corpora.Conll as C
import NLP.Types
import NLP.Types.IOB

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
               , "[VP is/VBZ widely/RB expected/VBN to/TO take/VB] [NP another/DT]"
               ]
             )

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

mkGetSentencesTest :: (String, Text, [Text]) -> TestTree
mkGetSentencesTest (name, input, oracle) =
  testCase name (oracle @=? (map getText $ parseIOBSentences input))
