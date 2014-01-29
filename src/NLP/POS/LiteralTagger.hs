module NLP.POS.LiteralTagger
    ( tag
    , tagSentence
    , mkTagger
    , taggerID
    , readTagger
    )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.Map.Strict as Map
import Data.Serialize (encode, decode)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T

import NLP.Tokenize.Text (tokenize)
import NLP.FullStop (segment)
import NLP.Types ( tagUNK, Sentence, TaggedSentence
                 , Tag, POSTagger(..))

taggerID :: ByteString
taggerID = pack "NLP.POS.LiteralTagger"

-- | Create a Literal Tagger using the specified back-off tagger as a
-- fall-back, if one is specified.
--
-- This uses a tokenizer adapted from the 'tokenize' package for a
-- tokenizer, and Erik Kow's fullstop sentence segmenter as a sentence
-- splitter.
mkTagger :: Map Text Tag -> Maybe POSTagger -> POSTagger
mkTagger table mTgr = POSTagger { posTagger  = tag table
                                , posTrainer = \_ -> return $ mkTagger table mTgr
                                , posBackoff = mTgr
                                , posTokenizer = tokenize
                                , posSplitter = (map T.pack) . segment . T.unpack
                                , posSerialize = encode table
                                , posID = taggerID
                                }

tag :: Map Text Tag -> [Sentence] -> [TaggedSentence]
tag table ss = map (tagSentence table) ss

tagSentence :: Map Text Tag -> Sentence -> TaggedSentence
tagSentence table toks = map findTag toks
  where
    findTag :: Text -> (Text, Tag)
    findTag txt = (txt, Map.findWithDefault tagUNK txt table)

readTagger :: ByteString -> Maybe POSTagger -> Either String POSTagger
readTagger bs backoff = do
  model <- decode bs
  return $ mkTagger model backoff