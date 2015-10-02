{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Types
 ( module NLP.Types
 , module NLP.Types.Classes
 , module NLP.Types.POSTagger
 , module NLP.Types.Tags
 , module NLP.Types.General
 , module NLP.Types.Annotations
 , module NLP.Types.TokenizedSentence
 , module NLP.Types.TaggedSentence
 , module NLP.Types.ChunkedSentence
 , module NLP.Types.NERedSentence
 )
where

import Control.DeepSeq (NFData)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize (Serialize)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.List (foldl')

import GHC.Generics

import Test.QuickCheck.Arbitrary (Arbitrary(..))

import NLP.Types.General
import NLP.Types.Tags
import NLP.Types.POSTagger
import NLP.Types.Classes
import NLP.Types.Annotations
import NLP.Types.TokenizedSentence
import NLP.Types.TaggedSentence
import NLP.Types.ChunkedSentence
import NLP.Types.NERedSentence

-- | Document corpus.
--
-- This is a simple hashed corpus, the document content is not stored.
data Corpus = Corpus { corpLength     :: Int
                     -- ^ The number of documents in the corpus.
                     , corpTermCounts :: Map Text Int
                     -- ^ A count of the number of documents each term occurred in.
                     } deriving (Read, Show, Eq, Ord, Generic)

instance NFData Corpus
instance Serialize Corpus

instance Arbitrary Corpus where
  arbitrary = do
      docs <- arbitrary
      return $ mkCorpus docs

-- | Get the number of documents that a term occurred in.
termCounts :: Corpus -> Text -> Int
termCounts corpus term = Map.findWithDefault 0 term $ corpTermCounts corpus

-- | Add a document to the corpus.
--
-- This can be dangerous if the documents are pre-processed
-- differently.  All corpus-related functions assume that the
-- documents have all been tokenized and the tokens normalized, in the
-- same way.
addDocument :: Corpus -> [Text] -> Corpus
addDocument (Corpus count m) doc = Corpus (count + 1) (foldl' addTerm m doc)

-- | Create a corpus from a list of documents, represented by
-- normalized tokens.
mkCorpus :: [[Text]] -> Corpus
mkCorpus docs =
  let docSets = map Set.fromList docs
  in Corpus { corpLength     = length docs
            , corpTermCounts = foldl' addTerms Map.empty docSets
            }

addTerms :: Map Text Int -> Set Text -> Map Text Int
addTerms m terms = Set.foldl' addTerm m terms

addTerm :: Map Text Int -> Text -> Map Text Int
addTerm m term = Map.alter increment term m
  where
    increment :: Maybe Int -> Maybe Int
    increment Nothing  = Just 1
    increment (Just i) = Just (i + 1)
