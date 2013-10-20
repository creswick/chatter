{-# LANGUAGE OverloadedStrings #-}
module NLP.Types
where

import Data.Text (Text)

data POSTagger = POSTagger
    { tagger  :: [Sentence] -> [TaggedSentence] -- ^ The initial part-of-speech tagger.
    , backoff :: Maybe POSTagger   -- ^ A tagger to invoke on unknown tokens.
    }

type Sentence = [Text]
type TaggedSentence = [(Text, Tag)]

newtype Tag = Tag { fromTag :: Text
                  } deriving (Ord, Eq, Read, Show)

parseTag :: Text -> Tag
parseTag t = Tag t

-- | Constant tag for "unknown"
tagUNK :: Tag
tagUNK = Tag "Unk"
