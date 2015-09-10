{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module NLP.Types.TaggedSentence

where

import GHC.Generics
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Serialize (Serialize)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Safe (headMay, lastMay)
import Text.Read (readEither)

import Text.PrettyPrint (hsep, text)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint.HughesPJClass as HPJ
import Test.QuickCheck (Arbitrary(..), NonEmptyList(..))
import Test.QuickCheck.Instances ()

import NLP.Types.General
import NLP.Types.Annotations
import NLP.Types.TokenizedSentence
import NLP.Types.Classes

-- | Results of the POS tagger, which encompases a 'TokenizedSentence'
data TaggedSentence pos =
  TaggedSentence { tagTokSentence :: TokenizedSentence
                 , tagAnnotations :: [Annotation TokenizedSentence pos]
                 } deriving (Read, Show, Eq, Generic, Ord)

instance Hashable pos => Hashable (TaggedSentence pos)

instance AnnotatedText (TaggedSentence pos) where
  getText = getText . tagTokSentence

instance AnnotatedText (Annotation TokenizedSentence tag) where
  getText = getText . project

-- | Create an annotation that is over a lower-level version of a sequence of annotations.
project :: Annotation TokenizedSentence ann -> Annotation Text ann
project ann = let tokens = tokAnnotations $ payload ann

                  -- The index of the first (Annotation Text ann):
                  startTokIdx = fromIndex $ startIdx ann

                  -- The first (Annotation Text ann):
                  startTok = tokens !! startTokIdx
                  endTokIdx = (fromIndex $ startIdx ann) + ((len ann) - 1)
                  endTok = tokens !! endTokIdx

                  startix = fromIndex $ startIdx startTok
                  endix = (fromIndex $ startIdx endTok) + (len endTok)
              in Annotation { startIdx = startIdx startTok
                            , len = endix - startix
                            , value = value ann
                            , payload = tokText $ payload ann
                            }

instance (POS pos, HasMarkup pos) => Pretty (TaggedSentence pos) where
  pPrint (TaggedSentence (TokenizedSentence ts toks) anns) = text toStr
    where
      toStr = let (_, folded) = T.foldl' fn (0,"") ts
              in case Map.lookup (T.length ts) insertions of
                   Nothing -> reverse folded
                   Just  m -> reverse ((reverse m) <> folded)

      fn :: (Int, String) -> Char -> (Int, String)
      fn (idx, acc) ch = let newIdx = idx + 1
                             markedAcc = case Map.lookup idx insertions of
                                           Nothing -> acc
                                           Just m  -> (reverse m) <>acc
                         in (newIdx, ch:markedAcc)

      insertions = tagInsertions Map.empty anns

tagInsertions :: (POS pos, HasMarkup pos)
              => Map Int String -> [Annotation TokenizedSentence pos] -> Map Int String
tagInsertions initMap anns = foldl' mkInsertions initMap anns
  where
    mkInsertions :: (POS pos, HasMarkup pos)
                 => Map Int String -> Annotation TokenizedSentence pos -> Map Int String
    mkInsertions theMap ann@(Annotation (Index sIdx) annLen pos (TokenizedSentence txt toks)) =
      let (pfx, sfx) = getAnnotationMarkup ann
          sTxtIdx = fromIndex $ startIdx (toks!!sIdx)
          eTok = toks!!(sIdx + annLen - 1) -- -1 to account for length.
          eTxtIdx = (fromIndex $ startIdx eTok) + len eTok
      in Map.insertWith (\new old -> new <> old) eTxtIdx sfx
           (Map.insertWith (\new old -> old <> new) sTxtIdx pfx theMap)

-- | Count the length of the tokens of a 'TaggedSentence'.
--
-- Note that this is *probably* the number of annotations also, but it
-- is not necessarily the same.
tsLength :: POS pos => TaggedSentence pos -> Int
tsLength = length . tagAnnotations

-- | Generate a list of Tokens and their corresponding POS tags.
-- Creates a token for each POS tag, just in case any POS tags are
-- annotated over multiple tokens.
tsToPairs :: POS pos => TaggedSentence pos -> [(Token, pos)]
tsToPairs ts = mapMaybe (getToken $ tagTokSentence ts) (tagAnnotations ts)
  where
    getToken :: TokenizedSentence -> Annotation TokenizedSentence pos -> Maybe (Token, pos)
    getToken toksent ann = do
      let sIdx = fromIndex $ startIdx ann
          l = len ann
          toks = take l $ drop sIdx $ tokAnnotations toksent
      firstTok <- headMay toks
      let firstTokIdx = fromIndex $ startIdx firstTok
      lastToken <- lastMay toks
      let lastTokenEndIdx = (fromIndex $ startIdx lastToken) + (len lastToken)
          theTokenText = T.drop firstTokIdx $ T.take lastTokenEndIdx $ (getText toksent)
      return (Token theTokenText, value ann)

-- | Apply a parallel list of POS tags to a 'TokenizedSentence'
applyTags :: POS pos => TokenizedSentence -> [pos] -> TaggedSentence pos
applyTags ts tags = TaggedSentence { tagTokSentence = ts
                                   , tagAnnotations = zipWith mkAnnotation [0..] tags
                                   }
  where
    mkAnnotation idx tag = Annotation { startIdx = Index idx
                                      , len = 1
                                      , value = tag
                                      , payload = ts
                                      }

-- | Extract the POS tags from a tagged sentence.
getTags :: POS pos => TaggedSentence pos -> [pos]
getTags = snd . unapplyTags

-- | Extract the POS tags from a tagged sentence, returning the
-- tokenized sentence that they applied to.
unapplyTags :: POS pos => TaggedSentence pos -> (TokenizedSentence, [pos])
unapplyTags ts = (tagTokSentence ts, map value $ tagAnnotations ts)
