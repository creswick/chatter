{-# LANGUAGE OverloadedStrings #-}
module NLP.NER.LiteralNER
  ( tag
  , tagSentence
  , mkNER
  , taggerID
  , readTagger
  , CaseSensitive(..)
  , NERTagger(..)
  )
where

import           Control.Monad ((>=>))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Serialize (encode, decode)

import           NLP.Tokenize.Annotations ( runTokenizer, RawToken(..)
                                          , protectTerms,  defaultTokenizer)
import           NLP.Types
import qualified NLP.POS as POS

taggerID :: ByteString
taggerID = "NLP.NER.LiteralNER"

data NERTagger pos chunk ner = NERTagger
  { nerTagger :: Text -> [NERedSentence pos chunk ner]
  , nerTrainer :: [NERedSentence pos chunk ner]
               -> IO (NERTagger pos chunk ner)
  , nerBackoff :: Maybe (NERTagger pos chunk ner)
  , nerPosTagger :: POSTagger pos
  , nerSerialize :: ByteString
  , nerID :: ByteString
  }

-- | Create a literal NER tagger.  Tags strings in the input 'Text'
-- with the provided tags.
--
mkNER :: (POS pos, Chunk chunk, NamedEntity ner)
      => Map Text ner
      -- ^ The map of tokens to tag, with their NER tags.
      -> CaseSensitive
      -- ^ Are tokens case-sensitive?
      -> POSTagger pos
      -- ^ The POSTagger to use for *both* tokenization and POS
      -- tagging prior to NER (using the `posTokenizer`)
      -> Maybe (NERTagger pos chunk ner)
      -- ^ Backoff-NER tagger, used to tag things that are not in the
      -- map.
      -> NERTagger pos chunk ner
mkNER table sensitive pTagger mTgr = NERTagger
  { nerTagger = \txt -> tag (canonicalize table) sensitive (POS.tag newPosTagger txt)
  , nerTrainer = \_ -> return $ mkNER table sensitive pTagger mTgr
  , nerBackoff = mTgr
  , nerPosTagger = newPosTagger
  , nerSerialize = encode (table, sensitive, posSerialize newPosTagger)
  , nerID = taggerID
  }
  where
    newPosTagger = pTagger {
                     posTokenizer = protectedTokenizer
                   }

    protectedTokenizer :: RawToken -> [RawToken]
    protectedTokenizer =
      protectTerms (Map.keys table) sensitive >=> posTokenizer pTagger

    canonicalize =
      case sensitive of
        Sensitive   -> id
        Insensitive -> Map.mapKeys T.toLower

tag :: (POS pos, Chunk chunk, NamedEntity ner)
    => Map Text ner
    -> CaseSensitive
    -> [TaggedSentence pos]
    -> [NERedSentence pos chunk ner]
tag table sensitive ss = map (tagSentence table sensitive) ss

tagSentence :: (POS pos, Chunk chunk, NamedEntity ner)
            => Map Text ner
            -> CaseSensitive
            -> TaggedSentence pos
            -> NERedSentence pos chunk ner
tagSentence table sensitive sent =
  NERedSentence { neChunkSentence = ChunkedSentence
                                    { chunkTagSentence = sent
                                    , chunkAnnotations = []
                                    }
                , neAnnotations = mapMaybe tagToken $ zip (tagAnnotations sent) [0..]
                }
  where

--    tagToken :: (Annotation Text Token, Int) -> (Annotation TokenizedSentence pos)
    tagToken (ann, idx) = do
      nerTag <- Map.lookup (canonicalize $ getText ann) table
      return $ Annotation { startIdx = Index idx
                          , len = 1
                          , value = nerTag
                          , payload = sent
                          }

    canonicalize :: Text -> Text
    canonicalize =
      case sensitive of
        Sensitive   -> id
        Insensitive -> T.toLower

-- | deserialization for Literal Taggers.  The serialization logic is
-- in the posSerialize record of the POSTagger created in mkTagger.
readTagger :: (POS pos, Chunk chunk, NamedEntity ner)
           => ByteString
           -> Maybe (NERTagger pos chunk ner)
           -> Either String (NERTagger pos chunk ner)
readTagger bs backoff = do
  (model, sensitive, pTaggerBS) <- decode bs
  pTagger <- POS.deserialize POS.taggerTable pTaggerBS
  return $ mkNER model sensitive pTagger backoff
