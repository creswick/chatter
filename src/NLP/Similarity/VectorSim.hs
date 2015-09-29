module NLP.Similarity.VectorSim where

import Prelude hiding (lookup)
import Data.DefaultMap (DefaultMap)
import qualified Data.DefaultMap as DM
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (elemIndices)

import NLP.Types

-- | An efficient (ish) representation for documents in the "bag of
-- words" sense.
newtype TermVector = TermVector (DefaultMap Text Double)

fromTV :: TermVector -> DefaultMap Text Double
fromTV (TermVector dm) = dm



-- | Generate a `TermVector` from a tokenized document.
mkVector :: Corpus -> [Text] -> TermVector
mkVector corpus doc = TermVector $ DM.fromList 0 $ Set.toList $
                        Set.map (\t->(t, tf_idf t doc corpus)) (Set.fromList doc)


-- | Invokes similarity on full strings, using `T.words` for
-- tokenization, and no stemming. The return value will be in the
-- range [0, 1]
--
-- There *must* be at least one document in the corpus.
sim :: Corpus -> Text -> Text -> Double
sim corpus doc1 doc2 = similarity corpus (T.words doc1) (T.words doc2)

-- | Determine how similar two documents are.
--
-- This function assumes that each document has been tokenized and (if
-- desired) stemmed/case-normalized.
--
-- This is a wrapper around `tvSim`, which is a *much* more efficient
-- implementation.  If you need to run similarity against any single
-- document more than once, then you should create `TermVector`s for
-- each of your documents and use `tvSim` instead of `similarity`.
-- 
-- The return value will be in the range [0, 1].
--
-- There *must* be at least one document in the corpus.
similarity :: Corpus -> [Text] -> [Text] -> Double
similarity corpus doc1 doc2 = let
  vec1 = mkVector corpus doc1
  vec2 = mkVector corpus doc2
  in tvSim vec1 vec2

-- | Determine how similar two documents are.
--
-- Calculates the similarity between two documents, represented as
-- `TermVectors`, returning a double in the range [0, 1] where 1 represents
-- "most similar".
tvSim :: TermVector -> TermVector -> Double
tvSim doc1 doc2 = let
  theCos = cosVec doc1 doc2
  in if isNaN theCos then 0 else theCos

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

cosVec :: TermVector -> TermVector -> Double
cosVec vec1 vec2 = let
  dp = dotProd vec1 vec2
  mag = (magnitude vec1 * magnitude vec2)
  in dp / mag

-- | Calculate the magnitude of a vector.
magnitude :: TermVector -> Double
magnitude v = sqrt $ DM.foldl acc 0 $ fromTV v
  where
    acc :: Double -> Double -> Double
    acc cur new = cur + (new ** 2)

-- | find the dot product of two vectors.
dotProd :: TermVector -> TermVector -> Double
dotProd xs ys = let
  terms = Set.fromList (keys xs) `Set.union` Set.fromList (keys ys)
  in Set.foldl (+) 0 (Set.map (\t -> (lookup t xs) * (lookup t ys)) terms)

keys :: TermVector -> [Text]
keys tv = DM.keys $ fromTV tv

lookup :: Text -> TermVector -> Double
lookup key tv = DM.lookup key $ fromTV tv

