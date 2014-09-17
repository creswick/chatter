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
import Data.Either (isRight)
import qualified Data.Text as T
import Data.Text (Text)
import Text.Parsec.Prim (parse, (<|>), try)
import Text.Parsec.Pos
import qualified Text.Parsec.Combinator as PC
----------------------------------------------------------------------
import NLP.Types
import NLP.Extraction.Parsec

import TestUtils

tests :: Test
tests = testGroup "NLP.Extraction.Parsec"
        [ testProperty "posTok extraction" prop_posTok
        , testProperty "anyToken" prop_anyToken
        , testProperty "followedBy" prop_followedBy
        , testGroup "Noun Phrase extractor" $
            map (genTest parseNounPhrase)
             [ ("Just NN", [("Dog", Tag "NN")]
                         , Just ("Dog", Tag "n-phr"))
             , ("DT NN", [("The", Tag "DT"), ("dog", Tag "NN")]
                       , Just ("The dog", Tag "n-phr"))
             , ("NN NN", [("Sunday", Tag "NN"), ("night", Tag "NN")]
                       , Just ("Sunday night", Tag "n-phr"))
             , ("JJ NN", [("beautiful", Tag "JJ"), ("game", Tag "NN")]
                       , Just ("beautiful game", Tag "n-phr"))
             , ("None - VB", [("jump", Tag "VB")]
                           , Nothing)
             ]
        ]

instance Arbitrary Tag where
  arbitrary = do
    NonEmpty str <- arbitrary
    return $ Tag $ T.pack str

instance Arbitrary TaggedSentence where
  arbitrary = listOf $ do
    NonEmpty tok <- arbitrary
    tag <- arbitrary
    return (T.pack tok, tag)

prop_posTok :: TaggedSentence -> Property
prop_posTok taggedSent = taggedSent /= [] ==>
  let (firstTok, firstTag) = head taggedSent
      Right actual = parse (posTok firstTag) "prop_posTag" taggedSent
  in (firstTok, firstTag) == actual

prop_anyToken :: TaggedSentence -> Property
prop_anyToken taggedSent = taggedSent /= [] ==>
  let actual = parse anyToken "prop_anyToken" taggedSent
  in isRight actual

prop_followedBy :: TaggedSentence -> Property
prop_followedBy taggedSent = taggedSent /= []
                          && not (contains taggedSent ".") ==>
  let (theToken, theTag) = (".", Tag ".")
      extractor          = followedBy anyToken $ txtTok Insensitive theToken
      Right actual       = parse extractor "prop_followedBy"
                             (taggedSent ++ [(theToken, theTag)])
  in (theToken, theTag) == actual


nounPhrase :: Extractor (Text, Tag)
nounPhrase = do
  nlist <- PC.many1 (try (posTok $ Tag "NN")
              <|> try (posTok $ Tag "DT")
                  <|> (posTok $ Tag "JJ"))
  let term = T.intercalate " " (map fst nlist)
  return (term, Tag "n-phr")

parseNounPhrase :: TaggedSentence -> Maybe (Text, Tag)
parseNounPhrase sent =
  case parse nounPhrase "parseNounPhrase Test" sent of
    Left  _ -> Nothing
    Right v -> Just v
