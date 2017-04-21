{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : NLP.Chunk
Description : Phrase Chunking facilities.
Copyright   : Rogan Creswick, 2014
Maintainer  : creswick@gmail.com
Stability   : experimental

NLP.Chunk aims to make phrasal chunking trivially easy -- it is the
corolary to NLP.POS.

The simplest way to try out chunking with Chatter is to open a repl
after installing chatter and try this:

>> import NLP.POS
>> import NLP.Chunk
>> tgr <- defaultTagger
>> chk <- defaultChunker
>> chunkText tgr chk "Monads are monoids in the category of endofunctors."
> "[NP Monads/NNS are/VBP monoids/NNS] [PP in/IN] [NP the/DT category/NN] [PP of/IN] [NP endofunctors/NNS] ./."

Note that it isn't perfect--phrase chunking is tricky, and the
'defaultTagger' and 'defaultChunker' aren't trained on the largest
training set (they use Conll 2000).  You can easily train more taggers
and chunkers using the APIs exposed here if you have the training data
to do so.

-}
module NLP.Chunk
  ( defaultTagger )
where

import           Codec.Compression.GZip      (decompress)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.List                   (isSuffixOf)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Serialize              (decode, encode)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           System.FilePath             ((</>))

import           NLP.POS                     (tag)
import           NLP.Types
import           NLP.Chunk.AvgPerceptronChunker (Chunker(..))
import qualified NLP.Chunk.AvgPerceptronChunker as Avg

import qualified NLP.Corpora.Conll as C

import           Paths_chatter

-- | A basic Phrasal chunker.
defaultChunker :: IO (Chunker C.Chunk C.Tag)
defaultChunker = conllChunker

-- | Convenient function to load the Conll2000 Chunker.
conllChunker :: IO (Chunker C.Chunk C.Tag)
conllChunker = do
  dir <- getDataDir
  loadChunker (dir </> "data" </> "models" </> "conll2000.chunk.model.gz")

-- | Train a chunker on a set of additional examples.
train :: (ChunkTag c, Tag t) => Chunker c t -> [ChunkedSentence c t] -> IO (Chunker c t)
train ch exs = chTrainer ch exs

-- | Chunk a 'TaggedSentence' that has been produced by a Chatter
-- tagger, producing a rich representation of the Chunks and the Tags
-- detected.
--
-- If you just want to see chunked output from standard text, you
-- probably want 'chunkText' or 'chunkStr'.
chunk :: (ChunkTag c, Tag t) => Chunker c t -> [TaggedSentence t] -> [ChunkedSentence c t]
chunk chk input = chChunker chk input


-- | Convenience funciton to Tokenize, POS-tag, then Chunk the
-- provided text, and format the result in an easy-to-read format.
--
-- > > tgr <- defaultTagger
-- > > chk <- defaultChunker
-- > > chunkText tgr chk "The brown dog jumped over the lazy cat."
-- > "[NP The/DT brown/NN dog/NN] [VP jumped/VBD] [NP over/IN the/DT lazy/JJ cat/NN] ./."
--
chunkText :: (ChunkTag c, Tag t) => POSTagger t -> Chunker c t -> Text -> Text
chunkText tgr chk input = T.intercalate " " $ map showChunkedSent $ chunk chk $ tag tgr input


-- | A wrapper around 'chunkText' that packs strings.
chunkStr :: (ChunkTag c, Tag t) => POSTagger t -> Chunker c t -> String -> String
chunkStr tgr chk str = T.unpack $ chunkText tgr chk $ T.pack str

-- | The default table of tagger IDs to readTagger functions.  Each
-- tagger packaged with Chatter should have an entry here.  By
-- convention, the IDs use are the fully qualified module name of the
-- tagger package.
chunkerTable :: (ChunkTag c, Tag t) => Map ByteString
               (ByteString -> Either String (Chunker c t))
chunkerTable = Map.fromList
  [ (Avg.chunkerID, Avg.readChunker)
  ]

-- | Store a 'Chunker' to disk.
saveChunker :: (ChunkTag c, Tag t) => Chunker c t -> FilePath -> IO ()
saveChunker chunker file = BS.writeFile file (serialize chunker)

-- | Load a 'Chunker' from disk, optionally gunzipping if
-- needed. (based on file extension)
loadChunker :: (ChunkTag c, Tag t) => FilePath -> IO (Chunker c t)
loadChunker file = do
  content <- getContent file
  case deserialize chunkerTable content of
    Left err -> error err
    Right chunker -> return chunker
  where
    getContent :: FilePath -> IO ByteString
    getContent f | ".gz" `isSuffixOf` file = fmap (LBS.toStrict . decompress) $ LBS.readFile f
                 | otherwise               = BS.readFile f


serialize :: (ChunkTag c, Tag t) => Chunker c t -> ByteString
serialize chunker = encode ( chId chunker, chSerialize chunker)

deserialize :: (ChunkTag c, Tag t) =>
               Map ByteString
                  (ByteString -> Either String (Chunker c t))
            -> ByteString
            -> Either String (Chunker c t)
deserialize table bs = do
  (theID, payload) <- decode bs
  case Map.lookup theID table of
    Nothing -> Left ("Could not find ID in Chunker function map: " ++ show theID)
    Just fn -> fn payload
