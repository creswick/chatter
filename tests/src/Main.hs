{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Test.HUnit      ( (@=?) )
import Test.QuickCheck ( Arbitrary(..), elements )
import Test.QuickCheck.Property ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework ( buildTest, testGroup, Test, defaultMain )
import Test.Framework.Skip (skip)

import NLP.Corpora.Parsing (readPOS)
import NLP.Types (POSTag(..), Tag(..), tagUNK, parseTag)
import NLP.POS (tagText, trainNew)

import qualified AvgPerceptronTests as APT
import Corpora

main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [ testGroup "readPOS" $
          map readPOSTest
           [ ("Basic corpora test 1", "Dear/jj Sirs/nns :/: Let/vb"
             , [("Dear", Tag "jj"),("Sirs", Tag "nns"),(":", Tag ":"),("Let", Tag "vb")])

           , ("Basic corpora test 2", "the/DT dog/NN jumped/VB"
             , [("the", Tag "DT"),("dog", Tag "NN"),("jumped", Tag "VB")])

           , ("More whitespace", " Dear/jj  Sirs/nns   :/: Let/vb   "
             , [("Dear", Tag "jj"),("Sirs", Tag "nns"),(":", Tag ":"),("Let", Tag "vb")])

           , ("Failure scenario: no tags", "Dear Sirs : Let"
             , [("Dear", tagUNK),("Sirs", tagUNK),(":", tagUNK),("Let", tagUNK)])

           , ("Empty string", "", [])
          ]

        , testGroup "parseTag" $
           map parseTagTest
            [ ("$-sign on known tag", "WP$", Tag "WP$")
            , ("Unknown tag", "NP-S", Tag "NP-S")
            , ("$ on unknown", "VBX-$", Tag "VBX-$")
           ]

        , testGroup "Train and tag"
          [ testGroup "miniCorpora1" $
            map (trainAndTagTest miniCorpora1)
             [ ("the dog jumped .", "the/DT dog/NN jumped/VB ./.") ]
          , testGroup "miniCorpora2" $
            map (trainAndTagTest miniCorpora1)
             [ ("the dog jumped .", "the/DT dog/NN jumped/VB ./.") ]
          , skip $ testGroup "brown CA01" $
            map (trainAndTagTestFileCorpus brownCA01)
             [ ("the dog jumped .", "the/at dog/nn jumped/Unk ./.") ]
          , skip $ testGroup "brown CA" $
            map (trainAndTagTestIO brownCA)
             [ ("the dog jumped .", "the/at dog/nn jumped/vbd ./.") ]
          ]
        , APT.tests
        ]


trainAndTagTestFileCorpus :: FilePath -> (Text, Text) -> Test
trainAndTagTestFileCorpus file args = buildTest $ do
  corpus <- T.readFile file
  return $ trainAndTagTest corpus args

trainAndTagTestIO :: IO Text -> (Text, Text) -> Test
trainAndTagTestIO corpora (input, oracle) = testCase (T.unpack input) $ do
  tagger <- trainNew =<< corpora
  oracle @=? tagText tagger input

trainAndTagTest :: Text -> (Text, Text) -> Test
trainAndTagTest corpora (input, oracle) = testCase (T.unpack input) $ do
  tagger <- trainNew corpora
  oracle @=? tagText tagger input

parseTagTest :: (String, Text, Tag) -> Test
parseTagTest = genTest parseTag

readPOSTest :: (String, Text, [(Text, Tag)]) -> Test
readPOSTest = genTest readPOS


genTest :: (Show a, Show b, Eq b) => (a -> b) -> (String, a, b) -> Test
genTest fn (descr, input, oracle) =
    testCase (descr++" input: "++show input) assert
        where assert = oracle @=? fn input

instance Arbitrary POSTag where
  arbitrary = do str <- arbitrary
                 elements [ Other (T.pack str)
                          , UNK -- ^ Unknown
                          , CC -- ^ Coordinating conjunction
                          , CD -- ^ Cardinal number
                          , DT -- ^ Determiner
                          , EX -- ^ Existential there
                          , FW -- ^ Foreign word
                          , IN -- ^ Preposition or subordinating conjunction
                          , JJ -- ^ Adjective
                          , JJR -- ^ Adjective, comparative
                          , JJS -- ^ Adjective, superlative
                          , LS -- ^ List item marker
                          , MD -- ^ Modal
                          , NN -- ^ Noun, singular or mass
                          , NNS -- ^ Noun, plural
                          , NNP -- ^ Proper noun, singular
                          , NNPS -- ^ Proper noun, plural
                          , PDT -- ^ Predeterminer
                          , POS -- ^ Possessive ending
                          , PRP -- ^ Personal pronoun
                          , PRPS -- ^ (PRP$) Possessive pronoun
                          , RB -- ^ Adverb
                          , RBR -- ^ Adverb, comparative
                          , RBS -- ^ Adverb, superlative
                          , RP -- ^ Particle
                          , SYM -- ^ Symbol
                          , TO -- ^ to
                          , UH -- ^ Interjection
                          , VB -- ^ Verb, base form
                          , VBD -- ^ Verb, past tense
                          , VBG -- ^ Verb, gerund or present participle
                          , VBN -- ^ Verb, past participle
                          , VBP -- ^ Verb, non-3rd person singular present
                          , VBZ -- ^ Verb, 3rd person singular present
                          , WDT -- ^ Wh-determiner
                          , WP -- ^ Wh-pronoun
                          , WPS -- ^ Possessive wh-pronoun (WP$)
                          , WRB]