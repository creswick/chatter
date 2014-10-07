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
             [ ("Just NN", TS [("Dog", RawTag "NN")]
                         , Just ("Dog", RawTag "n-phr"))
             , ("DT NN", TS [("The", RawTag "DT"), ("dog", RawTag "NN")]
                       , Just ("The dog", RawTag "n-phr"))
             , ("NN NN", TS [("Sunday", RawTag "NN"), ("night", RawTag "NN")]
                       , Just ("Sunday night", RawTag "n-phr"))
             , ("JJ NN", TS [("beautiful", RawTag "JJ"), ("game", RawTag "NN")]
                       , Just ("beautiful game", RawTag "n-phr"))
             , ("None - VB", TS [("jump", RawTag "VB")]
                           , Nothing)
             ]
        ]

instance Arbitrary RawTag where
  arbitrary = do
    NonEmpty str <- arbitrary
    return $ RawTag $ T.pack str

instance (Arbitrary t, Tag t) => Arbitrary (TaggedSentence t) where
  arbitrary = do toks <-  listOf $ do
                            NonEmpty tok <- arbitrary
                            tag <- arbitrary
                            return (T.pack tok, tag)
                 return $ TS toks

prop_posTok :: TaggedSentence RawTag -> Property
prop_posTok taggedSent = taggedSent /= TS [] ==>
  let (firstTok, firstTag) = head (unTS taggedSent)
      Right actual = parse (posTok firstTag) "prop_posTag" taggedSent
  in (firstTok, firstTag) == actual

prop_anyToken :: TaggedSentence RawTag -> Property
prop_anyToken taggedSent = taggedSent /= TS [] ==>
  let actual = parse anyToken "prop_anyToken" taggedSent
  in isRight actual

prop_followedBy :: TaggedSentence RawTag -> Property
prop_followedBy taggedSent = taggedSent /= TS []
                          && not (contains taggedSent ".") ==>
  let (theToken, theTag) = (".", RawTag ".")
      extractor          = followedBy anyToken $ txtTok Insensitive theToken
      Right actual       = parse extractor "prop_followedBy"
                             (tsConcat [taggedSent, TS [(theToken, theTag)]])
  in (theToken, theTag) == actual


parseNounPhrase :: TaggedSentence RawTag -> Maybe (Text, RawTag)
parseNounPhrase sent =
  case parse nounPhrase "parseNounPhrase Test" sent of
    Left  _ -> Nothing
    Right v -> Just v
