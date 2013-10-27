module NLP.POS.UnambiguousTagger where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import NLP.Types

import qualified NLP.POS.LiteralTagger as LT


mkTagger :: Map Text Tag -> Maybe POSTagger -> POSTagger
mkTagger table mTgr = let
  litTagger = LT.mkTagger table mTgr

  trainer :: [TaggedSentence] -> IO POSTagger
  trainer exs = do
    let newTable = train table exs
    return $ mkTagger newTable mTgr

  in litTagger { posTrainer = trainer }

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


