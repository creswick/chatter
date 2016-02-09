{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module NLP.Types.NERedSentence

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
import NLP.Types.TaggedSentence
import NLP.Types.ChunkedSentence
import NLP.Types.Classes

-- | A sentence that has been marked with named entities.
data NERedSentence pos chunk ne =
  NERedSentence { neChunkSentence :: ChunkedSentence pos chunk
                , neAnnotations :: [Annotation (TaggedSentence pos) ne]
                -- ^ These annotations are annotating the
                -- 'TaggedSentence' contained in the 'ChunkedSentence'
                } deriving (Read, Show, Eq, Generic, Ord)

instance (Hashable pos, Hashable chunk, Hashable ne) => Hashable (NERedSentence pos chunk ne)

instance AnnotatedText (NERedSentence pos chunk ne) where
  getText = getText . neChunkSentence

instance (POS pos, Chunk chunk, NamedEntity ne) => Pretty (NERedSentence pos chunk ne) where
  pPrint ns = text (T.unpack toStr)
    where
      baseTxt = getText ns
      toStr = let (lastIdx, folded) = T.foldl' fn (0,"") baseTxt
              in case Map.lookup lastIdx insertions of
                   Nothing -> T.reverse folded
                   Just m  -> T.reverse ((T.reverse m) <> folded)

      fn :: (Int, Text) -> Char -> (Int, Text)
      fn (idx, acc) ch = let newIdx = idx + 1
                             markedAcc = case Map.lookup idx insertions of
                                           Nothing -> acc
                                           Just m  -> (T.reverse m) <> acc
                         in (newIdx, T.append (T.pack [ch]) markedAcc)

      insertions = tagInsertions neMap (tagAnnotations $ chunkTagSentence $ neChunkSentence ns)
      neMap = neInsertions Map.empty $ neAnnotations ns


neInsertions :: (NamedEntity ne, HasMarkup ne, POS pos, HasMarkup pos)
                => Map Int Text -> [Annotation (TaggedSentence pos) ne] -> Map Int Text
neInsertions initMap anns = foldl' mkInsertions initMap anns
  where
    mkInsertions :: (HasMarkup pos, HasMarkup ne)
                 => Map Int Text -> Annotation (TaggedSentence pos) ne -> Map Int Text
    mkInsertions theMap ann@(Annotation (Index sIdx) l ne dat) =
      let (pfx, sfx) = getAnnotationMarkup ann

          -- POS annotations:
          tags = tagAnnotations dat

          -- starting index of the tag annotation that marks this chunk:
          sTagIdx = fromIndex $ startIdx (tags!!sIdx)
          eTag = tags!!(sIdx + len ann - 1) -- -1 to account for length.
          eTagIdx = (fromIndex $ startIdx eTag) + len eTag

          -- Token annotations
          toks :: [Annotation Text Token]
          toks = tokAnnotations $ tagTokSentence dat

          -- starting index of the token.  This is the index into the text string:
          sTokIdx = fromIndex $ startIdx (toks!!sTagIdx)
          eTok = toks!!(sTagIdx + len ann - 1) -- -1 to account for length.
          eTokIdx = (fromIndex $ startIdx eTok) + len eTok

      in Map.insertWith (\new old -> new <> old) eTokIdx (T.pack sfx)
           (Map.insertWith (\new old -> old <> new) sTokIdx (T.pack pfx) theMap)

