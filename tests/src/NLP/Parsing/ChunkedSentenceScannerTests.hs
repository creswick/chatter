{-# LANGUAGE OverloadedStrings #-}
module NLP.Parsing.ChunkedSentenceScannerTests

where

import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup, localOption)
import Test.Tasty.QuickCheck ( testProperty, Property, (==>), generate
                             , QuickCheckTests(..), QuickCheckMaxSize(..))
import Test.HUnit      ( (@=?), assertFailure )
import Test.Tasty.HUnit (testCase)
import Test.QuickCheck.Monadic
import Test.QuickCheck.Modifiers

import Data.List (group)
import Data.Serialize (decode, encode)
import Data.Text (Text)
import qualified Data.Text as T

import NLP.Types (Error)
import NLP.Types.Arbitrary
import NLP.Types.Annotations (prettyShow)
import NLP.Types.ChunkedSentence
import NLP.Parsing.ChunkedSentenceScanner
import qualified NLP.Corpora.Conll as C

tests :: TestTree
tests = testGroup "NLP.Parsing.ChunkedSentenceScannerTests"
        [ testGroup "specific tests" $
          map mkScannerTest
          [ ( "A[NP bad]test"
            , [ Tok 0 "A", ChunkStart 1 "NP", Tok 5 "bad"
              , ChunkEnd 8, Tok 9 "test"])
          ,  ( "A [NP better] test"
             -- 012345678901234567
             , [ Tok 0 "A", ChunkStart 2 "NP", Tok 6 "better"
               , ChunkEnd 12, Tok 14 "test"])
          ,  ( "[NP A/DT] better"
             -- 01234567890123456
             , [ ChunkStart 0 "NP", Tok 4 "A", Pos 5 "DT", ChunkEnd 8
               , Tok 10 "better" ])
          ,  ( "[NP A/DT] better/JJR [NP test/NN]"
             -- 012345678901234567890123456789012
             , [ ChunkStart 0 "NP", Tok 4 "A", Pos 5 "DT", ChunkEnd 8
               , Tok 10 "better", Pos 16 "JJR"
               , ChunkStart 21 "NP", Tok 25 "test", Pos 29 "NN", ChunkEnd 32])
          ]
        , testGroup "Parsing round-trips" (
          [ -- testProperty "ChunkedSentence pretty / parse loop" prop_chunkedSentenceRoundTrips
          ] ++
          (map mkChunkedSentenceRTTest
           [ "Just/RB a/DT sentence/NN with/IN no/DT annotations/NNS ./."
           , "A/DT simple/JJ test/NN ./."
           , "[NP A/DT] better/JJR [NP test/NN]"
           , "[NP A/DT] better/JJR [NP test/NN] ./."
           , "A/DT better/JJR [NP test/NN] ./."
           -- These following tests fail because of the way the parser / serializer handles whitespace:
           -- , ""
           -- , "[NP A/DT]  better/JJR [NP test/NN]" -- note extra space after first chunk.

           -- Each token must also have a POS annotation at this point:
           -- , "Just a sentence with no annotations."
           ]))
        ]

mkScannerTest :: (String, [Lexeme]) -> TestTree
mkScannerTest (input, expected) = testCase input $
  expected @=? alexScanTokens input

mkChunkedSentenceRTTest :: Text -> TestTree
mkChunkedSentenceRTTest input = testCase (T.unpack input) $
  let parsed :: Either Error (ChunkedSentence C.Tag C.Chunk)
      parsed = parseChunkedSentence input
  in Right input @=? (prettyShow `fmap` parsed)

prop_chunkedSentenceRoundTrips :: ChunkedSentence C.Tag C.Chunk -> Bool
prop_chunkedSentenceRoundTrips chunkSentence =
  let shown1 = prettyShow chunkSentence

      ecs :: Either Error (ChunkedSentence C.Tag C.Chunk)
      ecs = parseChunkedSentence shown1

  in case ecs of
       Left err -> False
       Right cs -> shown1 == (prettyShow cs)
