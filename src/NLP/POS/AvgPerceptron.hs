module NLP.POS.AvgPerceptron where

import Safe (headMay)

import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

-- | Average Perceptron implementation of Part of speech tagging,
-- adapted for Haskell from this python implementation:
--
-- http://honnibal.wordpress.com/2013/09/11/a-good-part-of-speechpos-tagger-in-about-200-lines-of-python/
--
-- Perceptron code:
--  https://github.com/sloria/TextBlob/blob/dev/text/_perceptron.py
--
type Feature = String
type Class = String

data Perceptron = Perceptron {
    -- | Each feature gets its own weight vector, so weights is a
    -- dict-of-dicts
    weights :: Map Feature (Map Class Int)

    , classes :: Set Class

    -- | The accumulated values, for the averaging. These will be
    -- keyed by feature/clas tuples
    , totals :: Map (Feature, Class) Int

    -- | The last time the feature was changed, for the averaging. Also
    -- keyed by feature/clas tuples
    -- (tstamps is short for timestamps)
    , tstamps :: Map (Feature, Class) Int

    -- | Number of instances seen
    , instances :: Int
    } deriving (Read, Show, Eq)

incrementInstances :: Perceptron -> Perceptron
incrementInstances p = p { instances = 1 + (instances p) }

getTimestamp :: Perceptron -> (Feature, Class) -> Int
getTimestamp p param = Map.findWithDefault 0 param (tstamps p)

getTotal :: Perceptron -> (Feature, Class) -> Int
getTotal p param = Map.findWithDefault 0 param (totals p)

getFeatureWeight :: Perceptron -> Feature -> Map Class Int
getFeatureWeight p f = Map.findWithDefault Map.empty f (weights p)


-- | Predict a class given a feature vector.
--
-- Ported from python:
-- > def predict(self, features):
-- >     '''Dot-product the features and current weights and return the best label.'''
-- >     scores = defaultdict(float)
-- >     for feat, value in features.items():
-- >         if feat not in self.weights or value == 0:
-- >             continue
-- >         weights = self.weights[feat]
-- >         for label, weight in weights.items():
-- >             scores[label] += value * weight
-- >     # Do a secondary alphabetic sort, for stability
-- >     return max(self.classes, key=lambda label: (scores[label], label))
--
predict :: Perceptron -> Map Feature Int -> Maybe Class
predict per features = -- now find highest ranked score in scores
    headMay (map fst $ sortBy (compare `on` fst) $ Map.toList finalScores)
    where
      finalScores :: Map Class Int
      finalScores = foldr fn Map.empty (Map.toList features)

      fn :: (Feature, Int) -> Map Class Int -> Map Class Int
      fn (f, 0) scores = scores
      fn (f, v) scores = case Map.lookup f (weights per) of
         Nothing  -> scores
         Just vec -> foldr (doProd v) scores (Map.toList vec)

      doProd :: Int -> (Class, Int) -> Map Class Int -> Map Class Int
      doProd value (label, weight) scores =
        Map.alter (updater (weight * value)) label scores

      updater :: Int -> Maybe Int -> Maybe Int
      updater newVal Nothing  = Just newVal
      updater newVal (Just v) = Just (v + newVal)

-- update(self, truth, guess, features)
--    ...
--         self.i += 1
--         if truth == guess:
--             return None
--         for f in features:
--             weights = self.weights.setdefault(f, {}) -- setdefault is Map.findWithDefault, and destructive.
--             upd_feat(truth, f, weights.get(truth, 0.0), 1.0)
--             upd_feat(guess, f, weights.get(guess, 0.0), -1.0)
--         return None
update :: Perceptron -> Class -> Class -> [Feature] -> Perceptron
update per truth guess features
  | truth == guess = incrementInstances per
  | otherwise      = foldr loopBody per features
    where
      loopBody :: Feature -> Perceptron -> Perceptron
      loopBody f p = let
        fweights  = getFeatureWeight p f
        cweight c = Map.findWithDefault 0 c fweights
        in upd_feat guess f (cweight guess) (-1)
             (upd_feat truth f (cweight truth) 1 p)

-- | ported from python:
-- > def update(self, truth, guess, features):
-- >        '''Update the feature weights.'''
-- >        def upd_feat(c, f, w, v):
-- >           param = (f, c)
-- >           self._totals[param] += (self.i - self._tstamps[param]) * w
-- >           self._tstamps[param] = self.i
-- >           self.weights[f][c] = w + v
upd_feat :: Class -> Class -> Int -> Int -> Perceptron -> Perceptron
upd_feat c f w v p = let
    newInstances = 1 + (instances p) -- increment the instance counter.

    -- self._totals[param] += (self.i - self._tstamps[param]) * w
    tmpTotal  = (getTotal p (f, c)) + ((newInstances - getTimestamp p (f, c)) * w)
    newTotals = Map.insert (f, c) tmpTotal (totals p)

    -- self._tstamps[param] = self.i
    newTstamps = Map.insert (f, c) newInstances (tstamps p)

    -- self.weights[f][c] = w + v
    newWeights = Map.insert f (Map.insert c (w + v) (getFeatureWeight p f)) (weights p)

    in p { totals = newTotals
         , tstamps = newTstamps
         , weights = newWeights }