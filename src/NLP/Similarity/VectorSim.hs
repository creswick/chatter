{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NLP.Similarity.VectorSim where

import Control.Applicative ((<$>))
import Prelude hiding (lookup)
import Data.DefaultMap (DefaultMap)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.DefaultMap as DM
import qualified Data.HashSet as HSet
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (elemIndices)
import GHC.Generics
import NLP.Types
import Control.DeepSeq (NFData(..), deepseq)

-- | An efficient (ish) representation for documents in the "bag of
-- words" sense.
newtype TermVector = TermVector (DefaultMap Text Double)
  deriving (Read, Show, Eq, Generic, NFData)

instance Arbitrary TermVector where
  arbitrary = do
    theMap <- arbitrary
    let zeroMap = theMap { DM.defDefault = 0 }
    return $ TermVector zeroMap

data Document = Document { docTermFrequencies :: HM.HashMap Text Int
                         , docTokens :: [Text]
                         }
  deriving (Read, Show, Eq, Generic)

instance NFData Document where
  rnf (Document f m) = f `deepseq` m `deepseq` ()

instance Arbitrary Document where
  arbitrary = mkDocument <$> arbitrary

-- | Make a document from a list of tokens.
mkDocument :: [Text] -> Document
mkDocument ts = Document mp ts
    where
        mp = foldr (\t -> HM.insertWith (+) t 1) HM.empty ts

-- | Access the underlying DefaultMap used to store term vector details.
fromTV :: TermVector -> DefaultMap Text Double
fromTV (TermVector dm) = dm

-- | Generate a `TermVector` from a tokenized document.
mkVector :: Corpus -> Document -> TermVector
mkVector corpus doc =
    TermVector $ DM.DefMap { DM.defDefault = 0
                           , DM.defMap = HM.mapWithKey (\t _ -> tf_idf t doc corpus) (docTermFrequencies doc)
                           }

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
  vec1 = mkVector corpus $ mkDocument doc1
  vec2 = mkVector corpus $ mkDocument doc2
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
tf :: Text -> Document -> Int
tf term doc = HM.lookupDefault 0 term (docTermFrequencies doc)

-- | Calculate the inverse document frequency.
--
-- The IDF is, roughly speaking, a measure of how popular a term is.
idf :: Text -> Corpus -> Double
idf term corpus = let
  docCount         = 1 + corpLength corpus
  containedInCount = 2 + termCounts corpus term
  in log (fromIntegral docCount / fromIntegral containedInCount)

-- | Calculate the tf*idf measure for a term given a document and a
-- corpus.
tf_idf :: Text -> Document -> Corpus -> Double
tf_idf term doc corpus = let
  freq = tf term doc
  result | freq == 0 = 0
         | otherwise = (fromIntegral freq) * idf term corpus
  in result

cosVec :: TermVector -> TermVector -> Double
cosVec vec1 vec2 = let
  dp = dotProd vec1 vec2
  mag = (magnitude vec1 * magnitude vec2)
  in dp / mag

-- | Add two term vectors. When a term is added, its value in each
-- vector is used (or that vector's default value is used if the term
-- is absent from the vector). The new term vector resulting from the
-- addition always uses a default value of zero.
addVectors :: TermVector -> TermVector -> TermVector
addVectors vec1 vec2 = TermVector (DM.unionWith (+) 0 (fromTV vec1) (fromTV vec2))

-- | A "zero vector" term vector (i.e. @addVector v zeroVector = v@).
zeroVector :: TermVector
zeroVector = TermVector (DM.empty 0)

-- | Negate a term vector.
negate :: TermVector -> TermVector
negate vec = TermVector (DM.map ((-1) *) $ fromTV vec)

-- | Add a list of term vectors.
sum :: [TermVector] -> TermVector
sum = foldr addVectors zeroVector

-- | Calculate the magnitude of a vector.
magnitude :: TermVector -> Double
magnitude v = sqrt $ DM.foldl acc 0 $ fromTV v
  where
    acc :: Double -> Double -> Double
    acc cur new = cur + (new ** 2)

-- | find the dot product of two vectors.
dotProd :: TermVector -> TermVector -> Double
dotProd xs ys = let
  -- Perhaps a bit more performance can be had if we ensure the first set
  -- in the union is the smaller one?
  -- https://hackage.haskell.org/package/unordered-containers-0.2.7.2/docs/Data-HashSet.html#v:union
  terms = vectorToSet xs `HSet.union` vectorToSet ys
  in HSet.foldl' (+) 0 (HSet.map (\t -> (lookup t xs) * (lookup t ys)) terms)
  where
    vectorToSet = HSet.fromMap . (HM.map (const ())) . DM.defMap . fromTV

keys :: TermVector -> [Text]
keys tv = DM.keys $ fromTV tv

lookup :: Text -> TermVector -> Double
lookup key tv = DM.lookup key $ fromTV tv
