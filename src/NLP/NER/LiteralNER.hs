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
import           NLP.Extraction.Parsec

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
  { nerTagger = \txt -> tag (preprocess table) sensitive (POS.tag pTagger txt)
  , nerTrainer = \_ -> return $ mkNER table sensitive pTagger mTgr
  , nerBackoff = mTgr
  , nerPosTagger = pTagger
  , nerSerialize = encode (table, sensitive, posSerialize pTagger)
  , nerID = taggerID
  }
  where
    preprocess :: NamedEntity ner
               => Map Text ner
               -> Map [Token] ner
    preprocess theTable =
      let tokenizer = posTokenizer pTagger
      in Map.mapKeys (tokens . runTokenizer tokenizer . canonicalize) theTable

    canonicalize =
      case sensitive of
        Sensitive   -> id
        Insensitive -> T.toLower

tag :: (POS pos, Chunk chunk, NamedEntity ner)
    => Map [Token] ner
    -> CaseSensitive  -- ^ Currently ignored.
    -> [TaggedSentence pos]
    -> [NERedSentence pos chunk ner]
tag table sensitive ss = map (tagSentence table) ss

tagSentence :: (POS pos, Chunk chunk, NamedEntity ner)
            => Map [Token] ner
            -> TaggedSentence pos
            -> NERedSentence pos chunk ner
tagSentence table sent =
  NERedSentence { neChunkSentence = ChunkedSentence
                                    { chunkTagSentence = sent
                                    , chunkAnnotations = []
                                    }
                , neAnnotations = unproject sent tokAnnotations
                }
  where
    -- tokAnnotations :: (NamedEntity ner)
    --                => [Annotation TokenizedSentence ner]
    tokAnnotations =
      case doParse (annotateAllTokens (Map.toList table)) (tagTokSentence sent) of
        Left  err -> []
        Right res -> res

    unproject :: (POS pos, NamedEntity ner)
              => TaggedSentence pos
              -> [Annotation TokenizedSentence ner]
              -> [Annotation (TaggedSentence pos) ner]
    unproject tagSent anns = map (setPayload tagSent) anns

    setPayload ::(POS pos, NamedEntity ner)
               => TaggedSentence pos
               -> Annotation TokenizedSentence ner
               -> Annotation (TaggedSentence pos) ner
    setPayload ts ann = Annotation {
                          startIdx = Index (fromIndex $ startIdx ann)
                        , len = len ann
                        , value = value ann
                        , payload = ts
                        }

    -- canonicalize :: Text -> Text
    -- canonicalize =
    --   case sensitive of
    --     Sensitive   -> id
    --     Insensitive -> T.toLower

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



----------------------------------------------------------------------
--
--  [a] -- ^ needles
--  [a] -- ^ haystack
--  [(Int, Int)] -- ^ Instances
--
