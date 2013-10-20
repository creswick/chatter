{-# LANGUAGE OverloadedStrings #-}
module NLP.Types
where

import Data.Text (Text)

data POSTagger = POSTagger
    { tagger  :: [Sentence] -> [TaggedSentence] -- ^ The initial part-of-speech tagger.
    , backoff :: Maybe POSTagger   -- ^ A tagger to invoke on unknown tokens.
    , tokenizer :: Text -> Sentence -- ^ A tokenizer; (`Data.Text.words` will work.)
    , sentSplitter :: Text -> [Text] -- ^ A sentence splitter.  If your input is formatted as
                                     -- one sentence per line, then use `Data.Text.lines`,
                                     -- otherwise try Erik Kow's fullstop library.
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
