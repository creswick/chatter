module NLP.Types.POSTagger

where

import Data.ByteString (ByteString)
import Data.Text
import NLP.Types.Annotations
import NLP.Tokenize.Annotations (RawToken)

-- | Part of Speech tagger, with back-off tagger.
--
-- A sequence of pos taggers can be assembled by using backoff
-- taggers.  When tagging text, the first tagger is run on the input,
-- possibly tagging some tokens as unknown ('Tag "Unk"').  The first
-- backoff tagger is then recursively invoked on the text to fill in
-- the unknown tags, but that may still leave some tokens marked with
-- 'Tag "Unk"'.  This process repeats until no more taggers are found.
-- (The current implementation is not very efficient in this
-- respect.).
--
-- Back off taggers are particularly useful when there is a set of
-- domain specific vernacular that a general purpose statistical
-- tagger does not know of.  A LitteralTagger can be created to map
-- terms to fixed POS tags, and then delegate the bulk of the text to
-- a statistical back off tagger, such as an AvgPerceptronTagger.
--
-- `POSTagger` values can be serialized and deserialized by using
-- `NLP.POS.serialize` and NLP.POS.deserialize`. This is a bit tricky
-- because the POSTagger abstracts away the implementation details of
-- the particular tagging algorithm, and the model for that tagger (if
-- any).  To support serialization, each POSTagger value must provide
-- a serialize value that can be used to generate a `ByteString`
-- representation of the model, as well as a unique id (also a
-- `ByteString`).  Furthermore, that ID must be added to a `Map
-- ByteString (ByteString -> Maybe POSTagger -> Either String
-- POSTagger)` that is provided to `deserialize`.  The function in the
-- map takes the output of `posSerialize`, and possibly a backoff
-- tagger, and reconstitutes the POSTagger that was serialized
-- (assigning the proper functions, setting up closures as needed,
-- etc.) Look at the source for `NLP.POS.taggerTable` and
-- `NLP.POS.UnambiguousTagger.readTagger` for examples.
--
data POSTagger pos = POSTagger
    { posTagger  :: [TokenizedSentence] -> [TaggedSentence pos]
    -- ^ The initial part-of-speech tagger.
    , posTrainer :: [TaggedSentence pos] -> IO (POSTagger pos)
    -- ^ Training function to train the immediate POS tagger.
    , posBackoff :: Maybe (POSTagger pos)
    -- ^ A tagger to invoke on unknown tokens.
    , posTokenizer :: RawToken -> [RawToken]
    -- ^ A tokenizer
    , posSplitter :: Text -> [Text] -- ^ A sentence splitter.  If your input is formatted as
                                    -- one sentence per line, then use `Data.Text.lines`,
                                    -- otherwise try Erik Kow's fullstop library.
    , posSerialize :: ByteString -- ^ Store this POS tagger to a
                                 -- bytestring.  This does /not/
                                 -- serialize the backoff taggers.
    , posID :: ByteString -- ^ A unique id that will identify the
                          -- algorithm used for this POS Tagger.  This
                          -- is used in deserialization
    }
