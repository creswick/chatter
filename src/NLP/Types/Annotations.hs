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
import NLP.Types.Classes

-- | Convert a pretty-printable value into a text string.
prettyShow :: Pretty p => p -> Text
prettyShow = T.pack . HPJ.prettyShow

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

getAnnotationMarkup :: HasMarkup tag => Annotation dat tag -> (String,String)
getAnnotationMarkup ann = getMarkup (value ann)

posMarkup :: POS pos => pos -> (String, String)
posMarkup t = ("","/" <> (T.unpack $ serializePOS t))

chunkMarkup :: Chunk chunk => chunk -> (String, String)
chunkMarkup t = ("[" <> (T.unpack $ serializeChunk t) <> " ", "]")

-- | Get the underlying text out of an annotation
instance AnnotatedText sentence => AnnotatedText (Annotation sentence tag) where
  getText (Annotation start count _ sent) = getSubText sent start count
