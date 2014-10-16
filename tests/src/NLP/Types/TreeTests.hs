{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module NLP.Types.TreeTests where

import Test.HUnit      ( (@=?), Assertion )

import Test.Framework ( testGroup, Test )
import Test.QuickCheck (Arbitrary(..), elements, (==>), Property)
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Text as T
import Data.Text (Text)

import qualified NLP.Corpora.Conll as C
import NLP.Types

tests :: Test
tests = testGroup "NLP.Types.Tree"
        [ testGroup "showChunkedSent" $ map mkShowChunkedSentTest
          [ ( ChunkedSent [ mkChunk C.NP
                             [ mkChink C.DT "The"
                             , mkChink C.NN "dog"
                             ]
                           , mkChunk C.VP [mkChink C.VBD "jumped"]
                           , mkChink C.Term "."
                           ]
             , "[NP The/DT dog/NN] [VP jumped/VBD] ./.")
          , ( ChunkedSent [mkChunk C.NP
                            [ mkChink C.DT "A"
                            , mkChink C.NN "chair"
                            ]
                           , mkChink C.Term "."
                           ]
            , "[NP A/DT chair/NN] ./.")
          , ( ChunkedSent [ mkChunk C.NP
                             [ mkChunk C.NP
                               [ mkChink C.DT "The"
                               , mkChink C.NN "dog"
                               ]
                             , mkChunk C.PP
                               [ mkChink C.PRP "over"
                               , mkChink C.NN "there"
                               ]
                             ]
                           , mkChunk C.VP [mkChink C.VBD "jumped"]
                           , mkChink C.Term "."
                           ]
            , "[NP [NP The/DT dog/NN] [PP over/PRP there/NN]] [VP jumped/VBD] ./.")
          ]
        ]

mkShowChunkedSentTest :: (ChunkTag c, Tag t) => (ChunkedSentence c t, Text) -> Test
mkShowChunkedSentTest (sent, oracle) = testCase (T.unpack oracle) (oracle @=? showChunkedSent sent)
