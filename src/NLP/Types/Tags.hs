{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.Types.Tags
where

import Data.Serialize (Serialize, get, put)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics

import Test.QuickCheck (Arbitrary(..), NonEmptyList(..))
import Test.QuickCheck.Instances ()

import NLP.Types.General (Error)

-- | The class of things that can be regarded as 'chunks'; Chunk tags
-- are much like POS tags, but should not be confused. Generally,
-- chunks distinguish between different phrasal categories (e.g.; Noun
-- Phrases, Verb Phrases, Prepositional Phrases, etc..)
class (Ord a, Eq a, Read a, Show a, Generic a, Serialize a) => ChunkTag a where
  fromChunk :: a -> Text
  parseChunk :: Text -> Either Error a
  notChunk :: a

-- | The class of POS Tags.
--
-- We use a typeclass here because POS tags just need a few things in
-- excess of equality (they also need to be serializable and human
-- readable).  Passing around all the constraints everywhere becomes a
-- hassle, and it's handy to have a uniform interface to the diferent
-- kinds of tag types.
--
-- This typeclass also allows for corpus-specific tags to be
-- distinguished; They have different semantics, so they should not be
-- merged.  That said, if you wish to create a unifying POS Tag set,
-- and mappings into that set, you can use the type system to ensure
-- that that is done correctly.
--
-- This /may/ get renamed to POSTag at some later date.
class (Ord a, Eq a, Read a, Show a, Generic a, Serialize a) => Tag a where
  fromTag :: a -> Text
  parseTag :: Text -> a
  tagUNK :: a
  tagTerm :: a -> Text
  startTag :: a
  endTag :: a
  -- | Check if a tag is a determiner tag.
  isDt :: a -> Bool

-- | A fall-back 'ChunkTag' instance, analogous to 'RawTag'
newtype RawChunk = RawChunk Text
  deriving (Ord, Eq, Read, Show, Generic)

instance Serialize RawChunk

instance ChunkTag RawChunk where
  fromChunk (RawChunk ch) = ch
  parseChunk txt = Right (RawChunk txt)
  notChunk = RawChunk "O"

-- | A fallback POS tag instance.
newtype RawTag = RawTag Text
  deriving (Ord, Eq, Read, Show, Generic)

instance Serialize RawTag

-- | Tag instance for unknown tagsets.
instance Tag RawTag where
  fromTag (RawTag t) = t

  parseTag t = RawTag t

  -- | Constant tag for "unknown"
  tagUNK = RawTag "Unk"

  tagTerm (RawTag t) = t

  startTag = RawTag "-START-"
  endTag = RawTag "-END-"

  isDt (RawTag tg) = tg == "DT"

instance Arbitrary RawTag where
  arbitrary = do
    NonEmpty str <- arbitrary
    return $ RawTag $ T.pack str

instance Serialize Text where
  put txt = put $ encodeUtf8 txt
  get     = fmap decodeUtf8 get

