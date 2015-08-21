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
