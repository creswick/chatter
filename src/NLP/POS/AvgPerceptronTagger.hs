{-# LANGUAGE OverloadedStrings #-}
-- | Avegeraged Perceptron Tagger
--
-- Adapted from the python implementation found here:
--  * https://github.com/sloria/textblob-aptagger/blob/master/textblob_aptagger/taggers.py
module NLP.POS.AvgPerceptronTagger where

import NLP.POS.AvgPerceptron
import NLP.Types

import Data.List (zipWith4)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

type Sentence = [Text]
type TaggedSentence = [(Text, Class)]

-- | start markers to ensure all features in context are valid,
-- even for the first "real" tokens.
startToks :: [Text]
startToks = ["-START-", "-START2-"]

-- | end markers to ensure all features are valid, even for
-- the last "real" tokens.
endToks :: [Text]
endToks = ["-END-", "-END2-"]


-- > def tag(self, corpus, tokenize=True):
-- >     '''Tags a string `corpus`.'''
-- >     # Assume untokenized corpus has \n between sentences and ' ' between words
-- >     s_split = nltk.sent_tokenize if tokenize else lambda t: t.split('\n')
-- >     w_split = nltk.word_tokenize if tokenize else lambda s: s.split()
-- >     def split_sents(corpus):
-- >         for s in s_split(corpus):
-- >             yield w_split(s)
-- >      prev, prev2 = self.START
-- >     tokens = []
-- >     for words in split_sents(corpus):
-- >         context = self.START + [self._normalize(w) for w in words] + self.END
-- >         for i, word in enumerate(words):
-- >             tag = self.tagdict.get(word)
-- >             if not tag:
-- >                 features = self._get_features(i, word, context, prev, prev2)
-- >                 tag = self.model.predict(features)
-- >             tokens.append((word, tag))
-- >             prev2 = prev
-- >             prev = tag
-- >     return tokens
tag :: Perceptron -> [Sentence] -> [TaggedSentence]
tag per corpus = map (tagSentence per) corpus

tagSentence :: Perceptron -> Sentence -> TaggedSentence
tagSentence per sent = let
  context = startToks ++ sent ++ endToks

  tags = (map (Class . T.unpack) startToks) ++ map (predictPos per) features

  features = [ getFeatures context i word prev1 prev2 |
             i <- [0..]
             , word <- sent
             , prev1 <- tail tags
             , prev2 <- tags
             ]
  in zip sent (drop 2 tags)

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
train :: Int -> Perceptron -> [[(Text, POSTag)]] -> Perceptron
train itr per examples = let
  rawPer = trainCls itr per $ toClassLst $ map unzip examples
  in averageWeights rawPer

toClassLst ::  [(Sentence, [POSTag])] -> [(Sentence, [Class])]
toClassLst tagged = map (\(x, y)->(x, map (Class . show) y)) tagged

trainCls :: Int -> Perceptron -> [(Sentence, [Class])] -> Perceptron
trainCls itr per examples = foldl trainSentence per $ trainingSet
  where
    -- | TODO need to shuffle these randomly, instead of just repeating
    trainingSet = concat $ take itr $ repeat examples

-- | Train on one sentence
--
-- Adapted from this portion of the Python train method:
--
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
trainSentence :: Perceptron -> (Sentence, [Class]) -> Perceptron
trainSentence per (sent, ts) = let
  context = startToks ++ sent ++ endToks

  tags = (map (Class . T.unpack) startToks) ++ ts ++ (map (Class . T.unpack) endToks)

  features = zipWith4 (getFeatures context)
                         [0..] -- index
                         sent  -- words
                         (tail tags) -- prev1
                         tags  -- prev2

  fn :: Perceptron -> (Map Feature Int, Class) -> Perceptron
  fn model (feats, truth) = let
    guess = predictPos model feats
    in update model truth guess $ Map.keys feats

  in foldl fn per (zip features ts)

predictPos :: Perceptron -> Map Feature Int -> Class
predictPos model feats = fromMaybe (Class "Unk") $ predict model feats

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
getFeatures :: [Text] -> Int -> Text -> Class -> Class -> Map Feature Int
getFeatures ctx idx word prev prev2 = let
  context = startToks ++ ctx ++ endToks

  i = idx + length startToks

  add :: Map Feature Int -> [Text] -> Map Feature Int
  add m args = Map.alter increment (Feat $ T.intercalate " " args) m

  increment :: Maybe Int -> Maybe Int
  increment Nothing  = Just 1
  increment (Just w) = Just (w + 1)

  features :: [[Text]]
  features = [ ["bias", ""]
             , ["i suffix", suffix word ]
             , ["i pref1", T.take 1 word ]
             , ["i-1 tag", T.pack $ show prev ]
             , ["i-2 tag", T.pack $ show prev2 ]
             , ["i tag+i-2 tag", T.pack $ show prev, T.pack $ show prev2 ]
             , ["i word", context!!i ]
             , ["i-1 tag+i word", T.pack $ show prev, context!!i ]
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
