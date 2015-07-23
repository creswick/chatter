module NLP.Types.Contexts

where


import NLP.Types.Annotations

data POSContext pos = POSContext
    { posTagger  :: POSTagger
    -- ^ The initial part-of-speech tagger.
    , posTrainer :: (Text, [Annotation Token], [Annotation pos]) -> IO (POSTagger pos)
    -- ^ Training function to train the immediate POS tagger.
    , posBackoff :: Maybe (POSTagger pos)
    -- ^ A tagger to invoke on unknown tokens.
    , posSerialize :: ByteString
    -- ^ Store this POS tagger to a bytestring.  This does /not/
    -- serialize the backoff taggers.
    , posID :: ByteString
    -- ^ A unique id that will identify the algorithm used for this
    -- POS Tagger.  This is used in deserialization
    }


-- | The type of Chunkers, incorporates chunking, training,
-- serilazitaion and unique IDs for deserialization.
data ChunkContext pos chunk = ChunkContext
  { chChunker :: Chunker
  , chTrainer :: (Text, [Annotation Token], [Annotation pos], [Annotation chunk])
              -> IO (Chunker pos chunk)
  , chBackoff :: Maybe (ChunkContext pos chunk)
  -- ^ Back-off chunker to apply to tokens marked 'out' of a chunk.
  , chSerialize :: ByteString
  , chId :: ByteString
  }

data NERContext pos ne = NERContext
  { nerTagger :: NERer
  , nerTrainer :: (Text, [Annotation Token], [Annotation pos], [Annotation ne])
               -> IO (NERContext pos ne)
  , nerBackoff :: Maybe (NERContext pos ne)
  -- ^ Back-off NER to apply to unmarked tokens.
  , nerSerialize :: ByteString
  , nerId :: ByteString
  }
