module NLP.Chunk
where

import           Codec.Compression.GZip      (decompress)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.List                   (isSuffixOf)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Serialize              (decode, encode)
import           System.FilePath             ((</>))

import           NLP.Types
import           NLP.Chunk.AvgPerceptronChunker
import qualified NLP.Chunk.AvgPerceptronChunker as Avg

import qualified NLP.Corpora.Conll as C

import           Paths_chatter

defaultChunker :: IO (Chunker C.Chunk C.Tag)
defaultChunker = conllChunker

conllChunker :: IO (Chunker C.Chunk C.Tag)
conllChunker = do
  dir <- getDataDir
  loadChunker (dir </> "data" </> "models" </> "conll2000.chunk.model.gz")

train :: (ChunkTag c, Tag t) => Chunker c t -> [ChunkedSentence c t] -> IO (Chunker c t)
train ch exs = chTrainer ch exs

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
