module NLP.Similarity.VectorSim where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (elemIndices)

type Corpus = [[Text]]

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
idf ::Eq a => a -> [[a]] -> Double
idf term corpus = let
  docCount = length corpus
  containedInCount = 1 + (length $ filter (elem term) corpus)
  in log (fromIntegral docCount / fromIntegral containedInCount)

-- | Calculate the tf*idf measure for a term given a document and a
-- corpus.
tf_idf :: Eq a => a -> [a] -> [[a]] -> Double
tf_idf term doc corp = let
  corpus = doc:corp
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