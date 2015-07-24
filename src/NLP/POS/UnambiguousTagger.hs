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

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize (encode, decode)
import Data.Text (Text)

import NLP.Tokenize (tokenize)
import NLP.Types

import qualified NLP.POS.LiteralTagger as LT

taggerID :: ByteString
taggerID = pack "NLP.POS.UnambiguousTagger"

readTagger :: Tag t => ByteString -> Maybe (POSTagger t) -> Either String (POSTagger t)
readTagger bs backoff = do
  model <- decode bs
  return $ mkTagger model backoff

-- | Create an unambiguous tagger, using the supplied 'Map' as a
-- source of tags.
mkTagger :: Tag t => Map Text t -> Maybe (POSTagger t) -> POSTagger t
mkTagger table mTgr = let
  litTagger = LT.mkTagger table LT.Sensitive mTgr

--  trainer :: Tag t => [TaggedSentence t] -> IO (POSTagger t)
  trainer exs = do
    let newTable = train table exs
    return $ mkTagger newTable mTgr

  in litTagger { posTrainer = trainer
               , posSerialize = encode table
               , posID = taggerID
               , posTokenizer = tokenize
               }

-- | Trainer method for unambiguous taggers.
train :: Tag t => Map Text t -> [TaggedSentence t] -> Map Text t
train table exs = let

--  pairs :: POS t
  pairs = concatMap unTS exs

--  trainOnPair :: Map Text t -> POS t -> Map Text t
  trainOnPair t (POS tag (Token txt)) = Map.alter (incorporate tag) txt t

--  incorporate :: t -> Maybe t -> Maybe t
  incorporate new Nothing                 = Just new
  incorporate new (Just old) | new == old = Just old
                             | otherwise  = Just tagUNK -- Forget the tag.

  in foldl trainOnPair table pairs

