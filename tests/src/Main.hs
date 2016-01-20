{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Text (Text)

import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty ( TestTree, defaultIngredients, defaultMainWithIngredients
                  , testGroup )
import Test.Tasty.Ingredients (Ingredient )
import Test.Tasty.Runners.AntXML ( antXMLRunner )

import NLP.Types (safeParsePOS, RawTag(..))

import qualified AvgPerceptronTests as APT
import qualified BackoffTaggerTests as Backoff
import qualified Data.DefaultMapTests as DefMap
-- import qualified IntegrationTests as IT
import qualified NLP.Corpora.BrownTests as Brown
import qualified NLP.Corpora.ConllTests as Conll
-- import qualified NLP.Extraction.ParsecTests as Parsec
import qualified NLP.POS.UnambiguousTaggerTests as UT
import qualified NLP.POS.LiteralTaggerTests as LT
import qualified NLP.POSTests as POS
import qualified NLP.Similarity.VectorSimTests as Vec
import qualified NLP.TypesTests as TypeTests
import qualified NLP.Types.AnnotationTests as AnnT
-- import qualified NLP.Types.IOBTests as IOB
-- import qualified NLP.Types.TreeTests as Tree
-- import qualified NLP.Chunk.AvgPerceptronChunkerTests as APC
import qualified NLP.TokenizeTests as Tok
import qualified NLP.Parsing.ChunkedSentenceScannerTests as CLEX

main :: IO ()
main = defaultMainWithIngredients ingredients tests

ingredients :: [Ingredient]
ingredients = antXMLRunner : defaultIngredients


tests :: TestTree
tests = testGroup "Tests"
        [ testGroup "parseTag" $
          [ testProperty "basic tag parsing" prop_parseTag]
        , APT.tests
        , Backoff.tests
        , Vec.tests
        , POS.tests
        , AnnT.tests
        , UT.tests
        , LT.tests
        -- , TypeTests.tests
        , DefMap.tests
-- todo        , Parsec.tests
-- todo       , IT.tests
        -- , Brown.tests
        -- , Conll.tests
-- todo       , IOB.tests
-- todo        , APC.tests
-- todo       , Tree.tests
        , Tok.tests
        , CLEX.tests
        ]

prop_parseTag :: Text -> Bool
prop_parseTag txt = safeParsePOS txt == RawTag txt
