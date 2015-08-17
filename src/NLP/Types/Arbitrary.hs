{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NLP.Types.Arbitrary where

import Data.List (foldl')
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Gen (Gen(..), infiniteListOf, elements, suchThat)

import NLP.Types
import NLP.Tokenize (runTokenizer, whitespace)

-- import Debug.Trace

arbitraryPosInt :: Gen Int
arbitraryPosInt = do
  x :: Int
    <- arbitrary
  return (1 + abs x)

arbitraryText :: Gen Text
arbitraryText = do
  let chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++
              "!@#$%^&*()}{][-`~',.\"<>?+|;:"
  str <- infiniteListOf $ elements chars
  size <- arbitrary
  return $ T.pack $ take size str

arbitraryWS :: Gen Text
arbitraryWS = do
  let chars = " \n\r\t"
  str <- infiniteListOf $ elements chars
  size <- arbitraryPosInt
  return $ T.pack $ take size str

instance Arbitrary RawTag where
  arbitrary = do
    str <- suchThat arbitraryText (\x-> T.length x > 0)
    return $ RawTag str

instance Arbitrary TokenizedSentence where
  arbitrary = do
    tokCount <- arbitraryPosInt
    mkTokenizedSent tokCount

interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) (y:ys) = (x:y:interleave xs ys)

mkTokenizedSent :: Int -> Gen TokenizedSentence
mkTokenizedSent tokCount = do
  tokens <- infiniteListOf (arbitraryText `suchThat` (\t->T.length t > 0))
  wsToks <- infiniteListOf arbitraryWS

  let tsent = T.concat $ interleave (take tokCount tokens) wsToks
  return $ runTokenizer whitespace tsent

instance Arbitrary pos => Arbitrary (TaggedSentence pos) where
  arbitrary = do
    tokCount <- arbitraryPosInt
    mkTaggedSent tokCount

mkTaggedSent :: Arbitrary pos => Int -> Gen (TaggedSentence pos)
mkTaggedSent tokCount = do
  tsent <- mkTokenizedSent tokCount
  posTags <- infiniteListOf arbitrary

  let annotations = zipWith mkAnnotation
                    [0..]
                    (take tokCount posTags)
      mkAnnotation idx pos = Annotation { startIdx = Index idx
                                        , len = 1
                                        , value = pos
                                        , payload = tsent
                                        }
  return TaggedSentence { tagTokSentence = tsent
                        , tagAnnotations = annotations }


instance (Chunk chunk, POS pos, Arbitrary pos, Arbitrary chunk) => Arbitrary (ChunkedSentence pos chunk) where
  arbitrary = do
    indexes <- mkIndexes 0
    chunks <- infiniteListOf arbitrary
    dataCount <- arbitraryPosInt

    let rawData = take dataCount $ filter (\(_,c)->c /= notChunk) (zip indexes chunks)
        ((lastIdx, lastLen),_) = last rawData
        tokCount = lastIdx + lastLen + 1

    taggedSent <- mkTaggedSent tokCount
    let annotations = map mkAnnotation rawData

        mkAnnotation ((idx, len), chunk) = Annotation { startIdx = Index idx
                                                      , len = len
                                                      , value = chunk
                                                      , payload = taggedSent
                                                      }

    return ChunkedSentence {
                chunkTagSentence = taggedSent
              , chunkAnnotations = annotations }

mkIndexes :: Int -> Gen [(Int, Int)]
mkIndexes start = do
  l <- arbitrary `suchThat` (0 <)
  rest <- mkIndexes (start + l)
  return ((start, l):rest)
