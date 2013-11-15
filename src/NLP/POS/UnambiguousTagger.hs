-- | This POS tagger deterministically tags tokens.  However, if it
-- ever sees multiple tags for the same token, it will forget the tag
-- it has learned.  This is useful for creating taggers that have very
-- high precision, but very low recall.
--
-- Unambiguous taggers are also useful when defined with a
-- non-deterministic backoff tagger, such as an
-- "NLP.POS.AveragedPerceptronTagger", since the high-confidence tags
-- will be applied first, followed by the more non-deterministic
-- results of the backoff tagger.
module NLP.POS.UnambiguousTagger where

import Data.ByteString.Char8 (pack)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import NLP.Types

import qualified NLP.POS.LiteralTagger as LT

-- | Create an unambiguous tagger, using the supplied 'Map' as a
-- source of tags.
mkTagger :: Map Text Tag -> Maybe POSTagger -> POSTagger
mkTagger table mTgr = let
  litTagger = LT.mkTagger table mTgr

  trainer :: [TaggedSentence] -> IO POSTagger
  trainer exs = do
    let newTable = train table exs
    return $ mkTagger newTable mTgr

  in litTagger { posTrainer = trainer
               , posSerialize = return $ pack "<empty>"
               , posID = pack "NLP.POS.UnambiguousTagger"
               }

-- | Trainer method for unambiguous taggers.
train :: Map Text Tag -> [TaggedSentence] -> Map Text Tag
train table exs = let
  pairs :: [(Text, Tag)]
  pairs = concat exs

  trainOnPair :: Map Text Tag -> (Text, Tag) -> Map Text Tag
  trainOnPair t (txt, tag) = Map.alter (incorporate tag) txt t

  incorporate :: Tag -> Maybe Tag -> Maybe Tag
  incorporate new Nothing                 = Just new
  incorporate new (Just old) | new == old = Just old
                             | otherwise  = Nothing

  in foldl trainOnPair table pairs


