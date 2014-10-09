{-# LANGUAGE OverloadedStrings #-}
-- | Avegeraged Perceptron Tagger
--
-- Adapted from the python implementation found here:
--
--  * <https://github.com/sloria/textblob-aptagger/blob/master/textblob_aptagger/taggers.py>
--
module NLP.POS.AvgPerceptronTagger
  ( mkTagger
  , trainNew
  , trainOnFiles
  , train
  , trainInt
  , tag
  , tagSentence
  , emptyPerceptron
  , taggerID
  , readTagger
  )
where

import NLP.Corpora.Parsing (readPOSWith)
import NLP.POS.AvgPerceptron ( Perceptron, Feature(..)
                             , Class(..), predict, update
                             , emptyPerceptron, averageWeights)
import NLP.Types

import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.List (zipWith4, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Serialize (encode, decode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import NLP.Tokenize.Chatter (tokenize)
import NLP.FullStop (segment)
import System.Random.Shuffle (shuffleM)

taggerID :: ByteString
taggerID = pack "NLP.POS.AvgPerceptronTagger"

readTagger :: Tag t => ByteString -> Maybe (POSTagger t) -> Either String (POSTagger t)
readTagger bs backoff = do
  model <- decode bs
  return $ mkTagger model backoff

-- | Create an Averaged Perceptron Tagger using the specified back-off
-- tagger as a fall-back, if one is specified.
--
-- This uses a tokenizer adapted from the 'tokenize' package for a
-- tokenizer, and Erik Kow's fullstop sentence segmenter
-- (<http://hackage.haskell.org/package/fullstop>) as a sentence
-- splitter.
mkTagger :: Tag t => Perceptron -> Maybe (POSTagger t) -> POSTagger t
mkTagger per mTgr = POSTagger { posTagger  = tag per
                              , posTrainer = \exs -> do
                                  newPer <- trainInt itterations per exs
                                  return $ mkTagger newPer mTgr
                              , posBackoff = mTgr
                              , posTokenizer = tokenize
                              , posSplitter = (map T.pack) . segment . T.unpack
                              , posSerialize = encode per
                              , posID = taggerID
                              }

itterations :: Int
itterations = 5

-- | Train a new 'Perceptron'.
--
-- The training corpus should be a collection of sentences, one
-- sentence on each line, and with each token tagged with a part of
-- speech.
--
-- For example, the input:
--
-- > "The/DT dog/NN jumped/VB ./.\nThe/DT cat/NN slept/VB ./."
--
-- defines two training sentences.
--
-- >>> tagger <- trainNew "Dear/jj Sirs/nns :/: Let/vb\nUs/nn begin/vb\n"
-- >>> tag tagger $ map T.words $ T.lines "Dear sir"
-- "Dear/jj Sirs/nns :/: Let/vb"
--
trainNew :: Tag t => (Text -> t) -> Text -> IO Perceptron
trainNew parser rawCorpus = train parser emptyPerceptron rawCorpus

-- | Train a new 'Perceptron' on a corpus of files.
trainOnFiles :: Tag t => (Text -> t) -> [FilePath] -> IO Perceptron
trainOnFiles parser corpora = foldM step emptyPerceptron corpora
  where
    step :: Perceptron -> FilePath -> IO Perceptron
    step per path = do
      content <- T.readFile path
      train parser per content

-- | Add training examples to a perceptron.
--
-- >>> tagger <- train emptyPerceptron "Dear/jj Sirs/nns :/: Let/vb\nUs/nn begin/vb\n"
-- >>> tag tagger $ map T.words $ T.lines "Dear sir"
-- "Dear/jj Sirs/nns :/: Let/vb"
--
-- If you're using multiple input files, this can be useful to improve
-- performance (by folding over the files).  For example, see `trainOnFiles`
--
train :: Tag t => (Text -> t) -- ^ The POS tag parser.
      -> Perceptron -- ^ The inital model.
      -> Text       -- ^ Training data; formatted with one sentence
                    -- per line, and standard POS tags after each
                    -- space-delimeted token.
      -> IO Perceptron
train parse per rawCorpus = do
  let corpora = map (readPOSWith parse) $ T.lines rawCorpus
  trainInt itterations per corpora

-- | start markers to ensure all features in context are valid,
-- even for the first "real" tokens.
startToks :: [Token]
startToks = [Token "-START-", Token "-START2-"]

-- | end markers to ensure all features are valid, even for
-- the last "real" tokens.
endToks :: [Token]
endToks = [Token "-END-", Token "-END2-"]

-- | Tag a document (represented as a list of 'Sentence's) with a
-- trained 'Perceptron'
--
-- Ported from Python:
--
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
--
tag :: Tag t => Perceptron -> [Sentence] -> [TaggedSentence t]
tag per corpus = map (tagSentence per) corpus

-- | Tag a single sentence.
tagSentence :: Tag t => Perceptron -> Sentence -> TaggedSentence t
tagSentence per sent = let

  tags = (map tokenToClass startToks) ++ map (predictPos per) features

  features = zipWith4 (getFeatures sent)
             [0..]
             (tokens sent)
             (tail tags)
             tags

  in applyTags sent (map (\(Class c) -> parseTag $ T.pack c) $ drop 2 tags)

-- | Train a model from sentences.
--
-- Ported from Python:
--
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
--
trainInt :: Tag t =>
            Int -- ^ The number of times to iterate over the training
                -- data, randomly shuffling after each iteration. (@5@
                -- is a reasonable choice.)
         -> Perceptron -- ^ The 'Perceptron' to train.
         -> [TaggedSentence t] -- ^ The training data. (A list of @[(Text, Tag)]@'s)
         -> IO Perceptron    -- ^ A trained perceptron.  IO is needed
                             -- for randomization.
trainInt itr per examples = trainCls itr per $ toClassLst $ map unzipTags examples
  -- where
  --   toSentPair (xs, ts) = (Sent $ map Token xs, ts)

toClassLst :: Tag t => [(Sentence, [t])] -> [(Sentence, [Class])]
toClassLst tagged = map (\(x, y)->(x, map (Class . T.unpack . fromTag) y)) tagged

trainCls :: Int -> Perceptron -> [(Sentence, [Class])] -> IO Perceptron
trainCls itr per examples = do
  trainingSet <- shuffleM $ concat $ take itr $ repeat examples
  return $ averageWeights $ foldl' trainSentence per trainingSet

tokenToClass :: Token -> Class
tokenToClass = Class . T.unpack . showTok

-- | Train on one sentence.
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

  tags = (map tokenToClass startToks) ++ ts ++ (map tokenToClass endToks)

  features = zipWith4 (getFeatures sent)
                         [0..] -- index
                         (tokens sent)  -- words
                         (tail tags) -- prev1
                         tags  -- prev2

  fn :: Perceptron -> (Map Feature Int, Class) -> Perceptron
  fn model (feats, truth) = let
    guess = predictPos model feats
    in update model truth guess $ Map.keys feats

  in foldl' fn per (zip features ts)

-- | Predict a Part of Speech, defaulting to the @Unk@ tag, if no
-- classification is found.
predictPos :: Perceptron -> Map Feature Int -> Class
predictPos model feats = fromMaybe (Class "Unk") $ predict model feats

-- | Default feature set.
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
--
getFeatures :: Sentence -> Int -> Token -> Class -> Class -> Map Feature Int
getFeatures ctx idx word prev prev2 = let
  context = startToks ++ tokens ctx ++ endToks

  i = idx + length startToks

  add :: Map Feature Int -> [Text] -> Map Feature Int
  add m args = Map.alter increment (mkFeature $ T.intercalate " " args) m

  increment :: Maybe Int -> Maybe Int
  increment Nothing  = Just 1
  increment (Just w) = Just (w + 1)

  features :: [[Text]]
  features = [ ["bias", ""]
             , ["i suffix", suffix word ]
             , ["i pref1", T.take 1 $ showTok word ]
             , ["i-1 tag", T.pack $ show prev ]
             , ["i-2 tag", T.pack $ show prev2 ]
             , ["i tag+i-2 tag", T.pack $ show prev, T.pack $ show prev2 ]
             , ["i word",     showTok (context!!i) ]
             , ["i-1 tag+i word", T.pack $ show prev, showTok (context!!i) ]
             , ["i-1 word",   showTok (context!!(i-1)) ]
             , ["i-1 suffix",  suffix (context!!(i-1)) ]
             , ["i-2 word",   showTok (context!!(i-2)) ]
             , ["i+1 word",   showTok (context!!(i+1)) ]
             , ["i+1 suffix",  suffix (context!!(i+1)) ]
             , ["i+2 word",   showTok (context!!(i+2)) ]
             ]
  -- in trace ("getFeatures: "++show (ctx, idx, word, prev, prev2)) $
  in foldl' add Map.empty features

mkFeature :: Text -> Feature
mkFeature txt = Feat $ T.copy txt
