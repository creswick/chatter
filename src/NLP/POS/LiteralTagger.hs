module NLP.POS.LiteralTagger
    ( tag
    , tagSentence
    , mkTagger )
where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T

import NLP.FullStop (segment)
import NLP.Types ( tagUNK, Sentence, TaggedSentence
                 , Tag, POSTagger(..))


-- | Create a Literal Tagger using the specified back-off tagger as a
-- fall-back, if one is specified.
--
-- This defaults to using `Data.Text.words` for a tokenizer, and Erik
-- Kow's fullstop sentence segmenter as a sentence splitter.
mkTagger :: Map Text Tag -> Maybe POSTagger -> POSTagger
mkTagger table mTgr = POSTagger { posTagger  = tag table
                                , posTrainer = \_ -> return $ mkTagger table mTgr
                                , posBackoff = mTgr
                                , posTokenizer = T.words -- TODO replace with better tokenizer.
                                , posSplitter = (map T.pack) . segment . T.unpack
                                }

tag :: Map Text Tag -> [Sentence] -> [TaggedSentence]
tag table ss = map (tagSentence table) ss

tagSentence :: Map Text Tag -> Sentence -> TaggedSentence
tagSentence table toks = map findTag toks
  where
    findTag :: Text -> (Text, Tag)
    findTag txt = (txt, Map.findWithDefault tagUNK txt table)