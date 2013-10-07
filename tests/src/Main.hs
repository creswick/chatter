{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import Test.HUnit      ( (@=?) )
import Test.QuickCheck ( Arbitrary(..), Property, (==>), elements )
import Test.QuickCheck.Property ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.Framework ( testGroup, Test, defaultMain )

import NLP.Types (POSTag(..), parseTag)
import NLP.Corpora.Parsing (readPOS)

main :: IO ()
main = defaultMain tests


tests :: [Test]
tests = [testGroup "readPOS" $
          map readPOSTest
           [ ("Basic corpora test 1", "Dear/jj Sirs/nns :/: Let/vb"
             , [("Dear",JJ),("Sirs",NNS),(":",Other ":"),("Let",VB)])

           , ("Basic corpora test 2", "the/DT dog/NN jumped/VB"
             , [("the",DT),("dog",NN),("jumped",VB)])

           , ("More whitespace", " Dear/jj  Sirs/nns   :/: Let/vb   "
             , [("Dear",JJ),("Sirs",NNS),(":",Other ":"),("Let",VB)])

           , ("Failure scenario: no tags", "Dear Sirs : Let"
             , [("Dear",UNK),("Sirs",UNK),(":",UNK),("Let",UNK)])

           , ("Empty string", "", [])
          ]

        , testGroup "parseTag" $
           map parseTagTest
            [ ("$-sign on known tag", "WP$", WPS)
            , ("Unknown tag", "NP-S", Other "NP-S")
            , ("$ on unknown", "VBX-$", Other "VBX-$")
           ]
          ++ [
           testProperty "round-trip from data type" prop_parseTag
          ]
        ]


parseTagTest = genTest parseTag

prop_parseTag tag = case tag of
  Other str -> tag == parseTag str
  _         -> tag == parseTag (T.pack $ show tag)
    where types = tag :: POSTag

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