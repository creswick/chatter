{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.Types.TaggedSentence
  ( TaggedSentence(..)
  , tsLength
  , tsToPairs
  , applyTags
  , getTags
  , unapplyTags
  , tagInsertions
  )
where

import GHC.Generics
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Safe (headMay, lastMay)

import Text.PrettyPrint (text)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
-- import Test.QuickCheck.Instances ()

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

instance (POS pos, HasMarkup pos) => Pretty (TaggedSentence pos) where
  pPrint (TaggedSentence (TokenizedSentence _ toks) anns) = text (T.unpack toStr)
    where
      toStr :: Text
      toStr = T.intercalate " " $ zipWith pickEntry (map getText toks) [0..]

      pickEntry :: Text -> Int -> Text
      pickEntry txt idx = case Map.lookup idx insertions of
                            Nothing -> txt
                            Just  t -> t

      insertions = tagInsertions Map.empty anns

      -- | Create an annotation that is over a lower-level version of a sequence of annotations.
projectTokenized :: Annotation TokenizedSentence ann -> [Annotation Text ann]
projectTokenized ann =
  let -- The index of the first (Annotation Text ann):
      startTokIdx = fromIndex $ startIdx ann

      -- the list of tokens that this annotation ranges over.
      toks = take (len ann) $ drop startTokIdx $ tokAnnotations $ payload ann

      newAnnotation tokAnn = tokAnn { value = value ann }
  in map newAnnotation toks


instance AnnotatedText (Annotation TokenizedSentence tag) where
  getText theAnn = T.intercalate " " $ map getText $ projectTokenized theAnn

-- | INTERNAL
--
-- This is used by the ChunkedSentence pretty printer, so it's exposed.
--
-- It generates the list of offsets where something needs to be
-- inserted to mark out POS tags when serializing for pretty printing.
tagInsertions :: (POS pos, HasMarkup pos)
              => Map Int Text -> [Annotation TokenizedSentence pos] -> Map Int Text
tagInsertions initMap anns = foldl' mkInsertions initMap anns
  where
    mkInsertions :: (POS pos, HasMarkup pos)
                 => Map Int Text -> Annotation TokenizedSentence pos -> Map Int Text
    mkInsertions theMap ann@(Annotation (Index sIdx) annLen _pos (TokenizedSentence _txt toks)) =
      let (pfx, sfx) = getAnnotationMarkup ann
          startTok = getText (toks !! sIdx)

          endIdx = (sIdx + annLen) - 1
          endTok = getText (toks !! endIdx)

      -- These insertWith's bear a bit of explaining:
      --   They insert a full token (prefix + token) or (token + suffix), if nothing
      -- is in the map at that index yet.  If there *is* something there, then the token
      -- part doesn't need repeating, so we just prepend or append the prefix or suffix.
      in Map.insertWith (\_ old -> (T.pack pfx) <> old) sIdx (T.pack pfx <> startTok)
           (Map.insertWith (\_ old -> old <> (T.pack sfx)) endIdx (endTok <> T.pack sfx) theMap)

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
