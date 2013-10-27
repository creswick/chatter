{-# LANGUAGE OverloadedStrings #-}
module NLP.Types
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

data POSTagger = POSTagger
    { posTagger  :: [Sentence] -> [TaggedSentence] -- ^ The initial part-of-speech tagger.
    , posTrainer :: [TaggedSentence] -> IO POSTagger -- ^ Training function to train the immediate POS tagger.
    , posBackoff :: Maybe POSTagger    -- ^ A tagger to invoke on unknown tokens.
    , posTokenizer :: Text -> Sentence -- ^ A tokenizer; (`Data.Text.words` will work.)
    , posSplitter :: Text -> [Text] -- ^ A sentence splitter.  If your input is formatted as
                                    -- one sentence per line, then use `Data.Text.lines`,
                                    -- otherwise try Erik Kow's fullstop library.
    }

type Sentence = [Text]
type TaggedSentence = [(Text, Tag)]

-- | Remove the tags from a tagged sentence
stripTags :: TaggedSentence -> Sentence
stripTags = map fst

newtype Tag = Tag Text
  deriving (Ord, Eq, Read, Show)

fromTag :: Tag -> Text
fromTag (Tag t) = t

parseTag :: Text -> Tag
parseTag t = Tag t

-- | Constant tag for "unknown"
tagUNK :: Tag
tagUNK = Tag "Unk"


-- | Document corpus.
--
-- This is a simple hashed corpus, the document content is not stored.
data Corpus = Corpus { corpLength     :: Int
                     -- ^ The number of documents in the corpus.
                     , corpTermCounts :: Map Text Int
                     -- ^ A count of the number of documents each term occurred in.
                     } deriving (Read, Show, Eq, Ord)

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
addDocument (Corpus count m) doc = Corpus (count + 1) (foldl addTerm m doc)

-- | Create a corpus from a list of documents, represented by
-- normalized tokens.
mkCorpus :: [[Text]] -> Corpus
mkCorpus docs =
  let docSets = map Set.fromList docs
  in Corpus { corpLength     = length docs
            , corpTermCounts = foldl addTerms Map.empty docSets
            }

addTerms :: Map Text Int -> Set Text -> Map Text Int
addTerms m terms = Set.foldl addTerm m terms

addTerm :: Map Text Int -> Text -> Map Text Int
addTerm m term = Map.alter increment term m
  where
    increment :: Maybe Int -> Maybe Int
    increment Nothing  = Just 1
    increment (Just i) = Just (i + 1)
