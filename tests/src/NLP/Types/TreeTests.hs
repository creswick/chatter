{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module NLP.Types.TreeTests where

import Test.HUnit      ( (@=?) )
import Test.QuickCheck.Instances ()
import Test.Tasty.HUnit (testCase)
import Test.Tasty ( testGroup, TestTree )

import qualified Data.Text as T
import Data.Text (Text)

import qualified NLP.Corpora.Conll as C
import NLP.Types

tests :: TestTree
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

mkShowChunkedSentTest :: (ChunkTag c, Tag t) => (ChunkedSentence c t, Text) -> TestTree
mkShowChunkedSentTest (sent, oracle) = testCase (T.unpack oracle) (oracle @=? showChunkedSent sent)
