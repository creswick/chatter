module NLP.POS.LiteralTagger
    ( tag
    , tagSentence
    , mkTagger )
where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)

import NLP.Types ( tagUNK, Sentence, TaggedSentence
                 , Tag, POSTagger(..))


-- | Create a Literal Tagger using the specified back-off tagger as a
-- fall-back, if one is specified.
mkTagger :: Map Text Tag -> Maybe POSTagger -> POSTagger
mkTagger table mTgr = POSTagger { tagger  = tag table
                                , backoff = mTgr }

tag :: Map Text Tag -> [Sentence] -> [TaggedSentence]
tag table ss = map (tagSentence table) ss

tagSentence :: Map Text Tag -> Sentence -> TaggedSentence
tagSentence table toks = map findTag toks
  where
    findTag :: Text -> (Text, Tag)
    findTag txt = (txt, Map.findWithDefault tagUNK txt table)