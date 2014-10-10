{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module NLP.Types.IOBTests where

import Test.HUnit      ( (@=?), Assertion )

import Test.Framework ( testGroup, Test )
import Test.QuickCheck (Arbitrary(..), listOf, elements, NonEmptyList(..), (==>), Property)
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text (Text)

import qualified NLP.Corpora.Conll as C
import NLP.Types
import NLP.Types.IOB

tests :: Test
tests = testGroup "NLP.Types.IOB"
        [ testProperty "IOB Tags parse" prop_tagsParse
        , testGroup "getSentences" $ map mkGetSentencesTest
          [ ("One sentence", "This\nis\na\ntest\n."
            , [["This", "is", "a", "test", "."]])
          , ("No text", "", [])
          , ("Just whitespace", "  \n  ", [])
          , ("Two sentences", "He\njumped\n.\n\nShe\njumped\n."
            , [["He", "jumped", "."]
              ,["She","jumped", "."]])
          ]
        , testCase "IOB Corpus Test" (test_parseIOB iobExample)
        ]


data IOB = I | O | B deriving (Show)

instance Arbitrary IOB where
  arbitrary = elements [I, O, B]

stripTok :: Token -> Token
stripTok (Token t) = Token $ T.strip t

prop_tagsParse :: IOB -> Token -> C.Chunk -> C.Tag -> Property
prop_tagsParse iob (stripTok -> Token txt) chunk tag =
    T.empty /= txt && T.find isSpace txt == Nothing ==>
  -- IOB format doesn't /appear/ to allow spaces within tokens, hence the predicate above.
  let tok   = Token txt
      input = T.intercalate " " [ txt, fromTag tag
                    , T.intercalate "" [(T.pack $ show iob), "-", fromChunk chunk]]
      result :: Either Error (IOBChunk C.Chunk C.Tag)
      result = parseIOBLine input

      constr = case iob of
                 I -> IChunk (POS tag tok) chunk
                 B -> BChunk (POS tag tok) chunk
                 O -> OChunk (POS tag tok)
  in result == (Right constr)

test_parseIOB :: (ChunkTag c, Tag t) => (Text, [[IOBChunk c t]]) -> Assertion
test_parseIOB (txt, Right -> oracle) = oracle @=? parseIOB txt

iobExample :: (Text, [[IOBChunk C.Chunk C.Tag]])
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
             , [ [ BChunk (POS C.NN (Token "Confidence")) C.NP
                 , BChunk (POS C.IN (Token "in")) C.PP
                 , BChunk (POS C.DT (Token "the")) C.NP
                 , IChunk (POS C.NN (Token "pound")) C.NP
                 , OChunk (POS C.Term (Token "."))
                 ]
               , [ BChunk (POS C.VBZ (Token "is")) C.VP
                 , IChunk (POS C.RB  (Token "widely")) C.VP
                 , IChunk (POS C.VBN (Token "expected")) C.VP
                 , IChunk (POS C.TO (Token "to")) C.VP
                 , IChunk (POS C.VB (Token "take")) C.VP
                 , BChunk (POS C.DT (Token "another")) C.NP
                 ]
               ])


mkGetSentencesTest :: (String, Text, [[Text]]) -> Test
mkGetSentencesTest (name, input, oracle) =
  testCase name (oracle @=? getSentences input)
