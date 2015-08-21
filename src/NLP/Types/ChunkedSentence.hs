{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module NLP.Types.ChunkedSentence

where

import GHC.Generics
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.List (foldl', group)
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
import NLP.Types.Classes

-- | A 'Chunked' sentence, with underlying Part-of-Speech tags and tokens.
-- Note: This is not a deep tree, a separate parse tree is needed.
data ChunkedSentence pos chunk =
  ChunkedSentence { chunkTagSentence :: TaggedSentence pos
                  , chunkAnnotations :: [Annotation (TaggedSentence pos) chunk]
                  } deriving (Read, Show, Eq, Generic, Ord)

instance (Hashable pos, Hashable chunk) => Hashable (ChunkedSentence pos chunk)

chunkInsertions :: (Chunk chunk, HasMarkup chunk, POS pos, HasMarkup pos)
                => Map Int String -> [Annotation (TaggedSentence pos) chunk] -> Map Int String
chunkInsertions initMap anns = foldl' mkInsertions initMap anns
  where
    mkInsertions :: (HasMarkup pos, HasMarkup chunk)
                 => Map Int String -> Annotation (TaggedSentence pos) chunk -> Map Int String
    mkInsertions theMap ann@(Annotation (Index sIdx) l chunk dat) =
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

      in Map.insertWith (\new old -> new <> old) eTokIdx sfx
           (Map.insertWith (\new old -> old <> new) sTokIdx pfx theMap)


-- | Build a ChunkedSentence from a list of chunks and a corresponding
-- TaggedSentence.  This is not quite like the TaggedSentence version
-- ('applyTags') because consequetive equal chunks denote branching in
-- the tree.  Also, note that this is lossy; consecutive chunk tags
-- *will* be collapsed when creating a 'ChunkedSentence' this way.
toChunkedSentence :: (Chunk chunk, POS tag) => TaggedSentence tag -> [chunk] -> ChunkedSentence tag chunk
toChunkedSentence taggedSentence chunks =
  let groups = map (\g -> (head g, length g)) $ group chunks

      mkAnnotation (idx, acc) (chunk, chunkLen) | chunk == notChunk = (idx + chunkLen, acc)
                                                | otherwise         =
        let ann = Annotation { startIdx = Index idx
                             , len = chunkLen
                             , value = chunk
                             , payload = taggedSentence
                             }
        in ( idx + chunkLen, ann:acc )

  in ChunkedSentence
       { chunkTagSentence = taggedSentence
       , chunkAnnotations = reverse $ snd $ foldl' mkAnnotation (0,[]) groups
       }

-- | The dual of 'toChunkedSentence'.
--
-- This takes a 'ChunkedSentence' and removes the chunks, returning
-- the underlying tagged sentence paired with a list of parallel chunk
-- tags that apply to each POS tag in the 'TaggedSentence'.
fromChunkedSentence :: (Chunk chunk, POS pos)
                    => ChunkedSentence pos chunk
                    -> (TaggedSentence pos, [chunk])
fromChunkedSentence chunkedSent =
  let taggedSent = chunkTagSentence chunkedSent

      chunks = let (lastIdx, anns) = foldl fn (0,[]) (chunkAnnotations chunkedSent)
                   missingOs = (tsLength taggedSent) - 1 - lastIdx
                   lastOs = replicate missingOs notChunk
               in reverse (lastOs ++ anns)

      fn :: (Chunk chunk, POS pos) => (Int, [chunk]) -> Annotation (TaggedSentence pos) chunk -> (Int, [chunk])
      fn (idx, acc) ann =
        let outChunks = replicate ((fromIndex $ startIdx ann) - idx) notChunk
            newChunks = replicate (len ann) (value ann)
            newIdx = (fromIndex $ startIdx ann) + len ann

        in (newIdx, newChunks ++ outChunks ++ acc)

  in (taggedSent, chunks)

instance AnnotatedText (ChunkedSentence pos chunk) where
  getText = getText . chunkTagSentence


instance (Chunk chunk, POS pos) => Pretty (ChunkedSentence pos chunk) where
  pPrint cs = text toStr
    where
      ts = tokText $ tagTokSentence $ chunkTagSentence cs
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

      insertions = tagInsertions chunkMap (tagAnnotations $ chunkTagSentence cs)
      chunkMap = chunkInsertions Map.empty $ chunkAnnotations cs
