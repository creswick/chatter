{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances    #-}
module NLP.Extraction.ParsecTests where

----------------------------------------------------------------------
import Test.QuickCheck ( Arbitrary, arbitrary, (==>), Property
                       , NonEmptyList(..), listOf)
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework ( testGroup, Test )
----------------------------------------------------------------------
import qualified Data.Text as T
import Data.Text (Text)
import Text.Parsec.Prim (parse, (<|>), try)
import Text.Parsec.Pos
import qualified Text.Parsec.Combinator as PC
----------------------------------------------------------------------
import NLP.Types
import NLP.Extraction.Parsec
import NLP.Extraction.Examples.ParsecExamples

import TestUtils

tests :: Test
tests = testGroup "NLP.Extraction.Parsec"
        [ testProperty "posTok extraction" prop_posTok
        , testProperty "anyToken" prop_anyToken
        , testProperty "followedBy" prop_followedBy
        , testGroup "Noun Phrase extractor" $
            map (genTest parseNounPhrase)
             [ ("Just NN", TaggedSent [(POS (RawTag "NN") "Dog")]
                         , Just (POS (RawTag "n-phr") "Dog"))
             , ("DT NN", TaggedSent [(POS (RawTag "DT") "The"), (POS (RawTag "NN") "dog")]
                       , Just (POS (RawTag "n-phr") "The dog"))
             , ("NN NN", TaggedSent [(POS (RawTag "NN") "Sunday"), (POS (RawTag "NN") "night")]
                       , Just (POS (RawTag "n-phr") "Sunday night"))
             , ("JJ NN", TaggedSent [(POS (RawTag "JJ") "beautiful"), (POS (RawTag "NN") "game")]
                       , Just (POS (RawTag "n-phr") "beautiful game"))
             , ("None - VB", TaggedSent [(POS (RawTag "VB") "jump")]
                           , Nothing)
             ]
        ]

prop_posTok :: TaggedSentence RawTag -> Property
prop_posTok taggedSent = taggedSent /= TaggedSent [] ==>
  let (POS firstTag firstTok) = head (unTS taggedSent)
      Right actual = parse (posTok firstTag) "prop_posTag" taggedSent
  in (POS firstTag firstTok) == actual

prop_anyToken :: TaggedSentence RawTag -> Property
prop_anyToken taggedSent = taggedSent /= TaggedSent [] ==>
  let actual = parse anyToken "prop_anyToken" taggedSent
  in isRight actual

prop_followedBy :: TaggedSentence RawTag -> Property
prop_followedBy taggedSent = taggedSent /= TaggedSent []
                          && not (contains taggedSent ".") ==>
  let (theToken, theTag) = (".", RawTag ".")
      extractor          = followedBy anyToken $ txtTok Insensitive theToken
      Right actual       = parse extractor "prop_followedBy"
                             (tsConcat [taggedSent, TaggedSent [POS theTag theToken]])
  in (POS theTag theToken) == actual


parseNounPhrase :: TaggedSentence RawTag -> Maybe (POS RawTag)
parseNounPhrase sent =
  case parse nounPhrase "parseNounPhrase Test" sent of
    Left  _ -> Nothing
    Right v -> Just v
