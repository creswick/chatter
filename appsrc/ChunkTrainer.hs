{-# LANGUAGE OverloadedStrings #-}
module ChunkTrainer where

import Control.Applicative ((<$>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

import NLP.ML.AvgPerceptron (emptyPerceptron)
import qualified NLP.Chunk.AvgPerceptronChunker as Avg
import NLP.Chunk (train, saveChunker)
import NLP.Types.IOB

import qualified NLP.Corpora.Conll as C

main :: IO ()
main = do
  args <- getArgs
  let output = last args
      corpora = init args

      avgPerChunker :: Avg.Chunker C.Tag C.Chunk
      avgPerChunker = Avg.mkChunker emptyPerceptron

  rawCorpus <- T.concat <$> mapM T.readFile corpora
  let eChunkedSentences = parseToChunkedSentences rawCorpus
  case eChunkedSentences of
    Left             err -> T.putStrLn err
    Right chunkedCorpora -> do
      chunker <- train avgPerChunker chunkedCorpora
      saveChunker chunker output

