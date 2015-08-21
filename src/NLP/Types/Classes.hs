{-# LANGUAGE DeriveGeneric #-}
module NLP.Types.Classes

where

import GHC.Generics
import Data.Hashable (Hashable)
import Data.List (foldl', group)
import Data.Map (Map)
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

import NLP.Types.General (toEitherErr, Error)

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
class (HasMarkup a, Ord a, Eq a, Read a, Show a, Generic a, Serialize a, Hashable a) => POS a where

  -- | Serialize a POS to a text representation.  eg: "NN", "VB", etc..
  -- This is the dual of `parsePOS`
  serializePOS :: a -> Text

  -- | Parse a POS tag into a structured POS value. (eg: "NN", "VB", etc..)
  -- This is the dual of `serializePOS`
  parsePOS :: Text -> Either Error a

  safeParsePOS :: Text -> a
  safeParsePOS txt = case parsePOS txt of
                       Left _ -> tagUNK
                       Right pos -> pos

  -- | The value used to represent "unknown".
  tagUNK :: a

  -- | Special marker POS for start of a corpus.
  startPOS :: a

  -- | Special marker POS for the end of a corpus.
  endPOS :: a

  -- | Check if a tag is a determiner tag.
  isDt :: a -> Bool

-- | The class of things that can be regarded as 'chunks'; Chunk tags
-- are much like POS tags, but should not be confused. Generally,
-- chunks distinguish between different phrasal categories (e.g.; Noun
-- Phrases, Verb Phrases, Prepositional Phrases, etc..)
class (HasMarkup a, Ord a, Eq a, Read a, Show a, Generic a, Serialize a, Hashable a) => Chunk a where
  -- | Serialize a chunk to a text representation (such as "NP", "VP", etc.)
  -- This is the dual of `parseChunk`.
  serializeChunk :: a -> Text

  -- | Parse a chunk from a text representation (such as "NP", "VP", etc.)
  -- This is the dual of `serializeChunk`.
  parseChunk :: Text -> Either Error a

  -- | Special chunk value to indicate something is not in a chunk.
  notChunk :: a

-- | The class of named entity sets.  This typeclass can be defined
-- entirely in terms of the required class constraints.
class (HasMarkup a, Ord a, Eq a, Read a, Show a, Generic a, Serialize a, Hashable a) => NamedEntity a where

  -- | Serialize a Named Entity to a Textual representation (eg:
  -- "MISC", "PER", "ORG", etc..)  This is the dual of `parseNETag`.
  serializeNETag :: a -> Text
  serializeNETag = T.pack . show

  -- | Parse a Named Entity from a textual representation (eg: "MISC",
  -- "PER", "ORG", etc..)  This is the dual of `serializeNETag`.
  parseNETag :: Text -> Either Error a
  parseNETag txt = toEitherErr $ readEither $ T.unpack txt

class Show a => HasMarkup a where
  getMarkup :: a -> (String, String)
  getMarkup a = ("["<> show a, "]")

-- | Safe index type, uses a phantom type to prevent us from indexing
-- into the wrong thing.
newtype Index a = Index Int
  deriving (Read, Show, Eq, Ord, Generic)

instance Hashable (Index a)

fromIndex :: Index a -> Int
fromIndex (Index x) = x

-- | Typeclass of things that have underlying text, so it's easy to
-- get the annotated document out of a tagged, tokenized, or chunked
-- result.
class AnnotatedText sentence where
  getText :: sentence -> Text

  getSubText :: sentence -> Index sentence -> Int -> Text
  getSubText txt (Index start) chars = T.take chars $ T.drop start $ getText txt

instance AnnotatedText Text where
  getText = id

