module NLP.Types.Annotations

where

import Data.Hashable (Hashable)

-- | Annotations are the base of all tags (POS tags, Chunks, marked
-- entities, etc.)
--
-- The semantics of the particular annotation depend on the type of
-- the value, and these can be wrapped up in a newtype for easier use.
data Annotation a =
  Annotation { startIdx :: Int
             -- ^ The starting index of the annotation.
             , endIdx :: Int
             -- ^ The end index of the annotation. The length of an
             -- annotation is therefore: (endIdx a - startIdx a).
             , value :: a
             -- ^ The value, such as a POS tag.
             } deriving (Read, Show, Eq, Hashable, Ord)

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
class (Ord a, Eq a, Read a, Show a, Generic a, Serialize a, Hashable a) => POS a where

  -- | Serialize a POS to a text representation.  eg: "NN", "VB", etc..
  -- This is the dual of `parsePOS`
  serializePOS :: a -> Text

  -- | Parse a POS tag into a structured POS value. (eg: "NN", "VB", etc..)
  -- This is the dual of `serializePOS`
  parsePOS :: Text -> Either Error a

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
class (Ord a, Eq a, Read a, Show a, Generic a, Serialize a, Hashable a) => Chunk a where
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
class (Ord a, Eq a, Read a, Show a, Generic a, Serialize a, Hashable a) => NamedEntity a where

  -- | Serialize a Named Entity to a Textual representation (eg:
  -- "MISC", "PER", "ORG", etc..)  This is the dual of `parseNETag`.
  serializeNETag :: a -> Text
  serializeNETag = T.pack . show

  -- | Parse a Named Entity from a textual representation (eg: "MISC",
  -- "PER", "ORG", etc..)  This is the dual of `serializeNETag`.
  parseNETag :: Text -> Either Error a
  parseNETag txt = toEitherErr $ readEither $ T.unpack txt
