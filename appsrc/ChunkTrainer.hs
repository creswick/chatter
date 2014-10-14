{-# LANGUAGE OverloadedStrings #-}
module ChunkTrainer where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

import NLP.POS.AvgPerceptron (emptyPerceptron)
import qualified NLP.Chunk.AvgPerceptronChunker as Avg
import NLP.Chunk (train, saveChunker)
import NLP.Types.IOB

import qualified NLP.Corpora.Conll as C

main :: IO ()
main = do
  args <- getArgs
  let output = last args
      corpora = init args

      avgPerChunker :: Avg.Chunker C.Chunk C.Tag
      avgPerChunker = Avg.mkChunker emptyPerceptron

  rawCorpus <- mapM T.readFile corpora
  let eChunkedCorpora = parseIOB $ T.concat rawCorpus
  case eChunkedCorpora of
    Left err -> T.putStrLn err
    Right chunkedCorpora -> do
      chunker <- train avgPerChunker $ map toChunkTree chunkedCorpora
      saveChunker chunker output

