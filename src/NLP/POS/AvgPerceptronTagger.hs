{-# LANGUAGE OverloadedStrings #-}
-- | Avegeraged Perceptron Tagger
--
-- Adapted from the python implementation found here:
--  * https://github.com/sloria/textblob-aptagger/blob/master/textblob_aptagger/taggers.py
module NLP.POS.AvgPerceptronTagger where

import NLP.POS.AvgPerceptron

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as T

type Sentence = [Text]

-- | start markers to ensure all features in context are valid,
-- even for the first "real" tokens.
startToks :: [Text]
startToks = ["-START-", "-START2-"]

-- | end markers to ensure all features are valid, even for
-- the last "real" tokens.
endToks :: [Text]
endToks = ["-END-", "-END2-"]

-- | Train a model from sentences, and save it at save_loc. nr_iter
-- controls the number of Perceptron training iterations.
--
-- :param sentences: A list of (words, tags) tuples.
-- :param save_loc: If not ``None``, saves a pickled model in this location.
-- :param nr_iter: Number of training iterations.
--
-- Ported from Python:
-- > def train(self, sentences, save_loc=None, nr_iter=5):
-- >     self._make_tagdict(sentences)
-- >     self.model.classes = self.classes
-- >     prev, prev2 = START
-- >     for iter_ in range(nr_iter):
-- >         c = 0
-- >         n = 0
-- >         for words, tags in sentences:
-- >             context = START + [self._normalize(w) for w in words] + END
-- >             for i, word in enumerate(words):
-- >                 guess = self.tagdict.get(word)
-- >                 if not guess:
-- >                     feats = self._get_features(i, word, context, prev, prev2)
-- >                     guess = self.model.predict(feats)
-- >                     self.model.update(tags[i], guess, feats)
-- >                 prev2 = prev; prev = guess
-- >                 c += guess == tags[i]
-- >                 n += 1
-- >         random.shuffle(sentences)
-- >         logging.info("Iter {0}: {1}/{2}={3}".format(iter_, c, n, _pc(c, n)))
-- >     self.model.average_weights()
-- >     # Pickle as a binary file
-- >     if save_loc is not None:
-- >         pickle.dump((self.model.weights, self.tagdict, self.classes),
-- >                      open(save_loc, 'wb'), -1)
-- >     return None
train :: Perceptron -> [Sentence] -> Int -> Perceptron
train = undefined

normalize = id

-- | Default feature set
--
-- > def _get_features(self, i, word, context, prev, prev2):
-- >     '''Map tokens into a feature representation, implemented as a
-- >     {hashable: float} dict. If the features change, a new model must be
-- >     trained.
-- >     '''
-- >     def add(name, *args):
-- >         features[' '.join((name,) + tuple(args))] += 1
-- >      i += len(self.START)
-- >     features = defaultdict(int)
-- >     # It's useful to have a constant feature, which acts sort of like a prior
-- >     add('bias')
-- >     add('i suffix', word[-3:])
-- >     add('i pref1', word[0])
-- >     add('i-1 tag', prev)
-- >     add('i-2 tag', prev2)
-- >     add('i tag+i-2 tag', prev, prev2)
-- >     add('i word', context[i])
-- >     add('i-1 tag+i word', prev, context[i])
-- >     add('i-1 word', context[i-1])
-- >     add('i-1 suffix', context[i-1][-3:])
-- >     add('i-2 word', context[i-2])
-- >     add('i+1 word', context[i+1])
-- >     add('i+1 suffix', context[i+1][-3:])
-- >     add('i+2 word', context[i+2])
-- >     return features
getFeatures :: Int -> Text -> [Text] -> Text -> Text -> Map Text Weight
getFeatures idx word ctx prev prev2 = let
  context = startToks ++ ctx ++ endToks

  i = idx + length startToks

  add :: Map Text Weight -> [Text] -> Map Text Weight
  add m args = Map.alter increment (T.intercalate " " args) m

  increment :: Maybe Weight -> Maybe Weight
  increment Nothing  = Just 1
  increment (Just w) = Just (w + 1)

  features :: [[Text]]
  features = [ ["bias", ""]
             , ["i suffix", suffix word ]
             , ["i pref1", T.take 1 word ]
             , ["i-1 tag", prev ]
             , ["i-2 tag", prev2 ]
             , ["i tag+i-2 tag", prev, prev2 ]
             , ["i word", context!!i ]
             , ["i-1 tag+i word", prev, context!!i ]
             , ["i-1 word", context!!(i-1) ]
             , ["i-1 suffix", suffix (context!!(i-1)) ]
             , ["i-2 word", context!!(i-2) ]
             , ["i+1 word", context!!(i+1) ]
             , ["i+1 suffix", suffix (context!!(i+1)) ]
             , ["i+2 word", context!!(i+2) ]
             ]

  in foldl add Map.empty features


suffix :: Text -> Text
suffix str | T.length str <= 3 = str
           | otherwise       = T.drop (T.length str - 3) str
