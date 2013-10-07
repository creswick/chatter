module NLP.POS.AvgPerceptron where

import Safe (headMay)

import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Text (Text)

-- | Average Perceptron implementation of Part of speech tagging,
-- adapted for Haskell from this python implementation:
--
-- http://honnibal.wordpress.com/2013/09/11/a-good-part-of-speechpos-tagger-in-about-200-lines-of-python/
--
-- Perceptron code:
--  https://github.com/sloria/TextBlob/blob/dev/text/_perceptron.py
--
newtype Feature = Feat Text
    deriving (Read, Show, Eq, Ord)

newtype Class = Class String
    deriving (Read, Show, Eq, Ord)

type Weight = Double

emptyPerceptron :: Perceptron
emptyPerceptron = Perceptron { weights = Map.empty
                             , classes = Set.empty
                             , totals = Map.empty
                             , tstamps = Map.empty
                             , instances = 0 }

data Perceptron = Perceptron {
    -- | Each feature gets its own weight vector, so weights is a
    -- dict-of-dicts
    weights :: Map Feature (Map Class Weight)

    , classes :: Set Class

    -- | The accumulated values, for the averaging. These will be
    -- keyed by feature/clas tuples
    , totals :: Map (Feature, Class) Weight

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

getTotal :: Perceptron -> (Feature, Class) -> Weight
getTotal p param = Map.findWithDefault 0 param (totals p)

getFeatureWeight :: Perceptron -> Feature -> Map Class Weight
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
predict per features = -- find highest ranked score in finalScores:
    headMay (map fst $ sortBy (compare `on` fst) $ Map.toList finalScores)
    where
      finalScores :: Map Class Weight
      finalScores = foldr fn Map.empty (Map.toList features)

      fn :: (Feature, Int) -> Map Class Weight -> Map Class Weight
      fn (f, 0) scores = scores
      fn (f, v) scores = case Map.lookup f (weights per) of
         Nothing  -> scores
         Just vec -> foldr (doProd v) scores (Map.toList vec)

      doProd :: Int -> (Class, Weight) -> Map Class Weight -> Map Class Weight
      doProd value (label, weight) scores =
        Map.alter (updater (weight * (fromIntegral value))) label scores

      updater :: Weight -> Maybe Weight -> Maybe Weight
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
upd_feat :: Class -> Feature -> Weight -> Weight -> Perceptron -> Perceptron
upd_feat c f w v p = let
    newInstances = 1 + (instances p) -- increment the instance counter.

    -- self._totals[param] += (self.i - self._tstamps[param]) * w
    paramTstamp = newInstances - getTimestamp p (f, c)
    tmpTotal  = (getTotal p (f, c)) + ((fromIntegral paramTstamp) * w)
    newTotals = Map.insert (f, c) tmpTotal (totals p)

    -- self._tstamps[param] = self.i
    newTstamps = Map.insert (f, c) newInstances (tstamps p)

    -- self.weights[f][c] = w + v
    newWeights = Map.insert f (Map.insert c (w + v) (getFeatureWeight p f)) (weights p)

    in p { totals = newTotals
         , tstamps = newTstamps
         , weights = newWeights }


-- | Average the weights?
--
-- Ported from Python:
-- > def average_weights(self):
-- >     for feat, weights in self.weights.items():
-- >         new_feat_weights = {}
-- >         for clas, weight in weights.items():
-- >             param = (feat, clas)
-- >             total = self._totals[param]
-- >             total += (self.i - self._tstamps[param]) * weight
-- >             averaged = round(total / float(self.i), 3)
-- >             if averaged:
-- >                 new_feat_weights[clas] = averaged
-- >         self.weights[feat] = new_feat_weights
-- >     return None
averageWeights :: Perceptron -> Perceptron
averageWeights per = per { weights = Map.mapWithKey avgWeights $ weights per }
  where
    avgWeights :: Feature -> Map Class Weight -> Map Class Weight
    avgWeights feat ws = Map.foldrWithKey (doAvg feat) Map.empty ws

    doAvg :: Feature -> Class -> Weight -> Map Class Weight -> Map Class Weight
    doAvg f c w acc = let
      param = (f, c)
      paramTotal = instances per - getTimestamp per param

      total :: Weight
      total = (getTotal per param) + ((fromIntegral paramTotal) * w)
      averaged = roundTo 3 (total / (fromIntegral $ instances per))
      in if 0 == averaged
           then acc
           else Map.insert c averaged acc

-- | round a fractional number to a specified decimal place.
roundTo :: RealFrac a => Int -> a -> a
roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)


-- Train a perceptron
--
-- Ported from Python:
-- > def train(nr_iter, examples):
-- >     model = Perceptron()
-- >     for i in range(nr_iter):
-- >         random.shuffle(examples)
-- >         for features, class_ in examples:
-- >             scores = model.predict(features)
-- >             guess, score = max(scores.items(), key=lambda i: i[1])
-- >             if guess != class_:
-- >                 model.update(class_, guess, features)
-- >     model.average_weights()
-- >     return model

