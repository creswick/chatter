{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances    #-}
module NLP.Extraction.ParsecTests where

----------------------------------------------------------------------
import Test.QuickCheck ((==>), Property)
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework ( testGroup, Test )
----------------------------------------------------------------------
import Text.Parsec.Prim (parse)
----------------------------------------------------------------------
import NLP.Types
import NLP.Extraction.Parsec
import NLP.Extraction.Examples.ParsecExamples
import qualified NLP.Corpora.Brown as B

import TestUtils

tests :: Test
tests = testGroup "NLP.Extraction.Parsec"
        [ testProperty "posTok extraction" prop_posTok
        , testProperty "anyToken" prop_anyToken
        , testProperty "followedBy" prop_followedBy
        , testGroup "Noun Phrase extractor" $
            map (genTest parseNounPhrase)
             [ ("Just NN", TaggedSent [POS B.NN "Dog"]
                         , Just (mkChunk B.C_NP [mkChink B.NN "Dog"]))
             , ("DT NN", TaggedSent [POS B.DT "The", POS B.NN "dog"]
                       , Just (mkChunk B.C_NP [ mkChink B.DT "The"
                                              , mkChink B.NN "dog"]))
             , ("NN NN", TaggedSent [POS B.NN "Sunday", POS B.NN "night"]
                       , Just (mkChunk B.C_NP [ mkChink B.NN "Sunday"
                                              , mkChink B.NN "night"]))
             , ("JJ NN", TaggedSent [POS B.JJ "beautiful", POS B.NN "game"]
                       , Just (mkChunk B.C_NP [ mkChink B.JJ "beautiful"
                                              , mkChink B.NN"game"]))
             , ("None - VB", TaggedSent [POS B.VB "jump"]
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


parseNounPhrase :: TaggedSentence B.Tag -> Maybe (ChunkOr B.Chunk B.Tag)
parseNounPhrase sent =
  case parse nounPhrase "parseNounPhrase Test" sent of
    Left  _ -> Nothing
    Right v -> Just v
