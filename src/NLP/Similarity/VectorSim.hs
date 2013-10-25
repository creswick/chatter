module NLP.Similarity.VectorSim where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (elemIndices)


-- | Document corpus.
--
-- Functions we'll need:
--  * length
--  * number of documents containing <term>
-- type Corpus = [[Text]]

data Corpus = Corpus { corpLength :: Int
                     , corpTermCounts :: Map Text Int
                     } deriving (Read, Show, Eq, Ord)

termCounts :: Corpus -> Text -> Int
termCounts corpus term = Map.findWithDefault 0 term $ corpTermCounts corpus

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

addDocument :: Corpus -> [Text] -> Corpus
addDocument (Corpus count m) doc = Corpus (count + 1) (foldl addTerm m doc)

-- | Invokes similarity on full strings, using `T.words` for
-- tokenization, and no stemming.
--
-- There *must* be at least one document in the corpus.
sim :: Corpus -> Text -> Text -> Double
sim corpus doc1 doc2 = similarity corpus (T.words doc1) (T.words doc2)

-- | Determine how similar two documents are.
--
-- This function assumes that each document has been tokenized and (if
-- desired) stemmed/case-normalized.
--
-- There *must* be at least one document in the corpus.
similarity :: Corpus -> [Text] -> [Text] -> Double
similarity corpus doc1 doc2 = let
  terms = Set.toList $ Set.fromList (doc1 ++ doc2)
  vec1 = map (\t->tf_idf t doc1 corpus) terms
  vec2 = map (\t->tf_idf t doc2 corpus) terms
  cos = cosVec vec1 vec2
  in if isNaN cos then 0 else cos

-- | Return the raw frequency of a term in a body of text.
--
-- The firt argument is the term to find, the second is a tokenized
-- document. This function does not do any stemming or additional text
-- modification.
tf :: Eq a => a -> [a] -> Int
tf term doc = length $ elemIndices term doc

-- | Calculate the inverse document frequency.
--
-- The IDF is, roughly speaking, a measure of how popular a term is.
idf :: Text -> Corpus -> Double
idf term corpus = let
  docCount = corpLength corpus
  containedInCount = 1 + termCounts corpus term
  in log (fromIntegral docCount / fromIntegral containedInCount)

-- | Calculate the tf*idf measure for a term given a document and a
-- corpus.
tf_idf :: Text -> [Text] -> Corpus -> Double
tf_idf term doc corp = let
  corpus = addDocument corp doc
  freq = tf term doc
  result | freq == 0 = 0
         | otherwise = (fromIntegral freq) * idf term corpus
  in result

-- | Find the cosine of the angle between two vectors.
--
-- The vectors must be the same length!
cosVec :: [Double] -> [Double] -> Double
cosVec vec1 vec2 = let
  dp = dotProd vec1 vec2
  mag = (magnitude vec1 * magnitude vec2)
  in dp / mag

magnitude :: [Double] -> Double
magnitude v = sqrt $ foldl acc 0 v
  where
    acc :: Double -> Double -> Double
    acc cur new = cur + (new ** 2)

-- | find the dot product of two vectors.
--
-- Vectors must be the same length! If they are not, the longer vector
-- will be truncated.
dotProd :: [Double] -> [Double] -> Double
dotProd xs ys = sum $ zipWith (*) xs ys