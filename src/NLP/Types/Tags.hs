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

class (Ord a, Eq a, Read a, Show a, Generic a, Serialize a) => ChunkTag a where
  fromChunk :: a -> Text

class (Ord a, Eq a, Read a, Show a, Generic a, Serialize a) => Tag a where
  fromTag :: a -> Text
  parseTag :: Text -> a
  tagUNK :: a
  tagTerm :: a -> Text

newtype RawChunk = RawChunk Text
  deriving (Ord, Eq, Read, Show, Generic)

instance Serialize RawChunk

instance ChunkTag RawChunk where
  fromChunk (RawChunk ch) = ch

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

instance Arbitrary RawTag where
  arbitrary = do
    NonEmpty str <- arbitrary
    return $ RawTag $ T.pack str

instance Serialize Text where
  put txt = put $ encodeUtf8 txt
  get     = fmap decodeUtf8 get

