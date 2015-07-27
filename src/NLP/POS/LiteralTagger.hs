{-# LANGUAGE OverloadedStrings #-}
module NLP.POS.LiteralTagger
    ( tag
    , tagSentence
    , mkTagger
    , taggerID
    , readTagger
    , CaseSensitive(..)
    )
where

import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.Map.Strict as Map
import Data.Serialize (encode, decode)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import NLP.Tokenize.Annotations (runTokenizer, protectTerms,  defaultTokenizer)
import NLP.FullStop (segment)
import NLP.Types ( tagUNK, Sentence, TaggedSentence(..), applyTags
                 , Tag, POSTagger(..), CaseSensitive(..), tokens, showTok)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text (compile)

taggerID :: ByteString
taggerID = pack "NLP.POS.LiteralTagger"


-- | Create a Literal Tagger using the specified back-off tagger as a
-- fall-back, if one is specified.
--
-- This uses a tokenizer adapted from the 'tokenize' package for a
-- tokenizer, and Erik Kow's fullstop sentence segmenter as a sentence
-- splitter.
mkTagger :: Tag t => Map Text t -> CaseSensitive -> Maybe (POSTagger t) -> POSTagger t
mkTagger table sensitive mTgr = POSTagger
  { posTagger  = tag (canonicalize table) sensitive
  , posTrainer = \_ -> return $ mkTagger table sensitive mTgr
  , posBackoff = mTgr
  , posTokenizer = runTokenizer (protectTerms (Map.keys table) sensitive >=> defaultTokenizer)
  , posSplitter = (map T.pack) . segment . T.unpack
  , posSerialize = encode (table, sensitive)
  , posID = taggerID
  }
  where canonicalize :: Tag t => Map Text t -> Map Text t
        canonicalize =
          case sensitive of
            Sensitive   -> id
            Insensitive -> Map.mapKeys T.toLower

tag :: Tag t => Map Text t -> CaseSensitive -> [Sentence] -> [TaggedSentence t]
tag table sensitive ss = map (tagSentence table sensitive) ss

tagSentence :: Tag t => Map Text t -> CaseSensitive -> Sentence -> TaggedSentence t
tagSentence table sensitive sent = applyTags sent (map findTag $ tokens sent)
  where
--    findTag :: Tag t => Token -> t
    findTag txt = Map.findWithDefault tagUNK (canonicalize $ showTok txt) table

    canonicalize :: Text -> Text
    canonicalize =
      case sensitive of
        Sensitive   -> id
        Insensitive -> T.toLower

-- | deserialization for Literal Taggers.  The serialization logic is
-- in the posSerialize record of the POSTagger created in mkTagger.
readTagger :: Tag t => ByteString -> Maybe (POSTagger t) -> Either String (POSTagger t)
readTagger bs backoff = do
  (model, sensitive) <- decode bs
  return $ mkTagger model sensitive backoff
