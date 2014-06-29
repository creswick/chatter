{-# LANGUAGE DeriveGeneric #-}
module NLP.POS.LiteralTagger
    ( tag
    , tagSentence
    , mkTagger
    , taggerID
    , readTagger
    , CaseSensitive(..)
    )
where


import GHC.Generics

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize, encode, decode)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T

import NLP.Tokenize.Text (tokenize)
import NLP.FullStop (segment)
import NLP.Types ( tagUNK, Sentence, TaggedSentence
                 , Tag, POSTagger(..))

taggerID :: ByteString
taggerID = pack "NLP.POS.LiteralTagger"

data CaseSensitive = Sensitive | Insensitive
  deriving (Read, Show, Generic)

instance Serialize CaseSensitive

-- | Create a Literal Tagger using the specified back-off tagger as a
-- fall-back, if one is specified.
--
-- This uses a tokenizer adapted from the 'tokenize' package for a
-- tokenizer, and Erik Kow's fullstop sentence segmenter as a sentence
-- splitter.
mkTagger :: Map Text Tag -> CaseSensitive -> Maybe POSTagger -> POSTagger
mkTagger table sensitive mTgr = POSTagger
  { posTagger  = tag (canonicalize table) sensitive
  , posTrainer = \_ -> return $ mkTagger table sensitive mTgr
  , posBackoff = mTgr
  , posTokenizer = tokenize
  , posSplitter = (map T.pack) . segment . T.unpack
  , posSerialize = encode (table, sensitive)
  , posID = taggerID
  }
  where canonicalize :: Map Text Tag -> Map Text Tag
        canonicalize =
          case sensitive of
            Sensitive   -> id
            Insensitive -> Map.mapKeys T.toLower

tag :: Map Text Tag -> CaseSensitive -> [Sentence] -> [TaggedSentence]
tag table sensitive ss = map (tagSentence table sensitive) ss

tagSentence :: Map Text Tag -> CaseSensitive -> Sentence -> TaggedSentence
tagSentence table sensitive toks = map findTag toks
  where
    findTag :: Text -> (Text, Tag)
    findTag txt = (txt, Map.findWithDefault tagUNK (canonicalize txt) table)

    canonicalize :: Text -> Text
    canonicalize =
      case sensitive of
        Sensitive   -> id
        Insensitive -> T.toLower

-- | deserialization for Literal Taggers.  The serialization logic is
-- in the posSerialize record of the POSTagger created in mkTagger.
readTagger :: ByteString -> Maybe POSTagger -> Either String POSTagger
readTagger bs backoff = do
  (model, sensitive) <- decode bs
  return $ mkTagger model sensitive backoff