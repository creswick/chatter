{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Chatter uses Annotations to represent marked portions of text, or
-- of underlying structures.
--
-- Annotations both add structure and provide commentary on elements
-- in that structure.  More concretely, consider the following text
-- string:
--
-- > "The blue boat sank in Lake Tahoe."
-- >  012345678901234567890123456789012
--
-- Imagine that we're trying to answer a simple question like "Where
-- did the boat sink?" There are a series of steps we need to take to
-- build sufficient structure to determine the answer.  The sentence
-- needs to be tokenized, tagged with parts-of-speech, chunked into
-- phrases, and (possibly) named entities located.
--
-- Each of these steps results in an 'Annotation' on the sentence.
--
-- 'Tokenizer's annotate tokens, marking where whitespace (or other
-- delimeters) separate interesting parts of the text. In this
-- example, the first token is:
--
-- > Annotation { startIdx = Index 0
-- >            , len = 3
-- >            , value = Token "The"
-- >            , payload = "The blue boat sank in Lake Tahoe."
-- >            }
--
-- Notice that annotations store the start and length of the regions
-- of the underlying payload that they annotate.  A sequence of these
-- 'Annotation's creates a 'TokenizedSentence'.
--
-- Annotations at a higher level, such as POS tags, are expressed over
-- 'TokenizedSentence's (or even higher-level structures).  For
-- example, POS-tagging this sentence results in (shown in plain-text for readibility):
--
-- > "The/DT blue/JJ boat/NN sank/VBD in/IN Lake/NNP Tahoe/NNP ./."
--
-- Each POS tag is represented as another annotation.  Here's the
-- first one:
--
-- > Annotation { startIdx = Index 0
-- >            , len = 1
-- >            , value = DT
-- >            , payload = TokenizedSentence {..}
-- >            }
--
-- In this case, the POS tags are not annotations on the input text,
-- instead, they are annotations on the 'TokenizedSentence'.  It's
-- still possible to get back to the underlying text, and POS tags
-- (and other annotations) can be linked back to the underlying input
-- data (so the initial input can be marked up visually, for example,
-- without loosing the delimeters or whitespace that is often taken
-- out during tokenization).
--
-- Annotations are polymorphic in the type of annotation used (eg:
-- 'Token' or a class of POS tag), as well as the underlying data that
-- is being annotated. (We do assume that the data is one-dimensional.)
--
-- This approach was inspired by the Apache UIMA use of Annotations
-- and Annotators for adding structure to unstructured data.
module NLP.Types.Annotations where

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

-- | Convert a pretty-printable value into a text string.
prettyShow :: Pretty p => p -> Text
prettyShow = T.pack . HPJ.prettyShow

-- | Safe index type, uses a phantom type to prevent us from indexing
-- into the wrong thing.
newtype Index a = Index Int
  deriving (Read, Show, Eq, Ord, Generic)

instance Hashable (Index a)

fromIndex :: Index a -> Int
fromIndex (Index x) = x

-- | Annotations are the base of all tags (POS tags, Chunks, marked
-- entities, etc.)
--
-- The semantics of the particular annotation depend on the type of
-- the value, and these can be wrapped up in a newtype for easier use.
data Annotation dat tag =
  Annotation { startIdx :: {-# UNPACK #-} !(Index dat)
             -- ^ The starting index of the annotation (a character
             -- offset into the underlying data).
             , len :: {-# UNPACK #-} !Int
             -- ^ The end index of the annotation.
             , value :: tag
             -- ^ The value, such as a POS tag.
             , payload :: dat
             -- ^ The underlying thing that is being annotated (such
             -- as a text string, or a list of other annotations)
             } deriving (Read, Show, Eq, Ord, Generic)

instance (Hashable dat, Hashable tag) => Hashable (Annotation dat tag)

instance AnnotatedText (Annotation Text Token) where
  getText ann = case value ann of
                  Token txt -> txt

instance AnnotatedText (Annotation TokenizedSentence pos) where
  getText ann = let tokens = tokAnnotations (payload ann)
                    text = tokText (payload ann)
                    startTok = tokens !! (fromIndex $ startIdx ann)
                    endTok = tokens !! (len ann + (fromIndex $ startIdx ann) - 1)
                    start = fromIndex $ startIdx startTok
                in T.take (len endTok) (T.drop start $ text)

getAnnotationMarkup :: HasMarkup tag => Annotation dat tag -> (String,String)
getAnnotationMarkup ann = getMarkup (value ann)

class Show a => HasMarkup a where
  getMarkup :: a -> (String, String)
  getMarkup a = ("["<> show a, "]")

posMarkup :: POS pos => pos -> (String, String)
posMarkup t = ("","/" <> (T.unpack $ serializePOS t))

chunkMarkup :: Chunk chunk => chunk -> (String, String)
chunkMarkup t = ("[" <> (T.unpack $ serializeChunk t) <> " ", "]")

instance HasMarkup Token where
  getMarkup ann = ("","")

-- | Wrapper around both the underlying text and the tokenizer results.
data TokenizedSentence =
  TokenizedSentence { tokText :: Text
                    , tokAnnotations :: [Annotation Text Token]
                    } deriving (Read, Show, Eq, Generic, Ord)

-- | Get the raw tokens out of a 'TokenizedSentence'
tokens :: TokenizedSentence -> [Token]
tokens ts = map value $ tokAnnotations ts

toTextToks :: TokenizedSentence -> [Text]
toTextToks ts = map getText $ tokAnnotations ts

instance Hashable TokenizedSentence
instance AnnotatedText TokenizedSentence where
  getText = tokText

-- | Results of the POS tagger, which encompases a 'TokenizedSentence'
data TaggedSentence pos =
  TaggedSentence { tagTokSentence :: TokenizedSentence
                 , tagAnnotations :: [Annotation TokenizedSentence pos]
                 } deriving (Read, Show, Eq, Generic, Ord)

instance Hashable pos => Hashable (TaggedSentence pos)

instance AnnotatedText (TaggedSentence pos) where
  getText = getText . tagTokSentence

-- | TODO this should use the underlying text, not stitching together.
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
tagInsertions initMap anns = foldl mkInsertions initMap anns
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

-- | A 'Chunked' sentence, with underlying Part-of-Speech tags and tokens.
-- Note: This is not a deep tree, a separate parse tree is needed.
data ChunkedSentence pos chunk =
  ChunkedSentence { chunkTagSentence :: TaggedSentence pos
                  , chunkAnnotations :: [Annotation (TaggedSentence pos) chunk]
                  } deriving (Read, Show, Eq, Generic, Ord)

instance (Hashable pos, Hashable chunk) => Hashable (ChunkedSentence pos chunk)

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

-- | Typeclass of things that have underlying text, so it's easy to
-- get the annotated document out of a tagged, tokenized, or chunked
-- result.
class AnnotatedText sentence where
  getText :: sentence -> Text

-- | Tokenization takes in text, produces annotations.
-- type Tokenizer = Text -> TokenizedSentence

-- | Chunking requires POS-tags (and tokenization) and generates annotations on the tokens.
-- type Chunker pos chunk = TaggedSentence pos -> ChunkedSentence pos chunk

-- | Named Entity recognition requires POS tags and tokens, and
-- produces annotations with Named Entities marked.
-- type NERer pos chunk ne = ChunkedSentence pos chunk -> NERedSentence pos chunk ne

-- | Sentinel value for tokens.
newtype Token = Token Text
  deriving (Read, Show, Eq, Hashable, Ord)

instance Pretty Token where
  pPrint (Token t) = text (T.unpack t)

-- | Unwrap the text of a 'Token'
showTok :: Token -> Text
showTok (Token t) = t

-- | Extract the last three characters of a 'Token', if the token is
-- long enough, otherwise returns the full token text.
suffix :: Token -> Text
suffix (Token str) | T.length str <= 3 = str
                   | otherwise         = T.drop (T.length str - 3) str

instance Arbitrary Token where
  arbitrary = do NonEmpty txt <- arbitrary
                 return $ Token (T.pack txt)

instance IsString Token where
  fromString = Token . T.pack


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
class (Ord a, Eq a, Read a, Show a, Generic a, Serialize a, Hashable a) => NamedEntity a where

  -- | Serialize a Named Entity to a Textual representation (eg:
  -- "MISC", "PER", "ORG", etc..)  This is the dual of `parseNETag`.
  serializeNETag :: a -> Text
  serializeNETag = T.pack . show

  -- | Parse a Named Entity from a textual representation (eg: "MISC",
  -- "PER", "ORG", etc..)  This is the dual of `serializeNETag`.
  parseNETag :: Text -> Either Error a
  parseNETag txt = toEitherErr $ readEither $ T.unpack txt
