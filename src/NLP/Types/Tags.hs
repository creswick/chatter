{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.Types.Tags
where

import Data.Hashable (Hashable)
import Data.Serialize (Serialize, get, put)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics

import Test.QuickCheck.Instances ()
import NLP.Types.General
import NLP.Types.Annotations
import NLP.Types.TokenizedSentence
import NLP.Types.TaggedSentence
import NLP.Types.ChunkedSentence
import NLP.Types.NERedSentence
import NLP.Types.Classes

-- | A fall-back 'Chunk' instance, analogous to 'RawTag'
newtype RawChunk = RawChunk Text
  deriving (Ord, Eq, Read, Show, Generic)

instance Serialize RawChunk
instance Hashable RawChunk

instance Chunk RawChunk where
  serializeChunk (RawChunk ch) = ch
  parseChunk txt = Right (RawChunk txt)
  notChunk = RawChunk "O"

instance HasMarkup RawChunk where
  getMarkup = chunkMarkup

-- | A fallback POS tag instance.
newtype RawTag = RawTag Text
  deriving (Ord, Eq, Read, Show, Generic)

instance Serialize RawTag
instance Hashable RawTag

instance HasMarkup RawTag where
  getMarkup = posMarkup

-- | Tag instance for unknown tagsets.
instance POS RawTag where
  serializePOS (RawTag t) = t

  parsePOS t = Right (RawTag t)

  -- | Constant tag for "unknown"
  tagUNK = RawTag "Unk"

  startPOS = RawTag "-START-"
  endPOS = RawTag "-END-"

  isDt (RawTag tg) = tg == "DT"

instance Serialize Text where
  put txt = put $ encodeUtf8 txt
  get     = fmap decodeUtf8 get

