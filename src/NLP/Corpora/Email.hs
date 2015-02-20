{-# LANGUAGE OverloadedStrings #-}
-- | Utilities for reading mailman-style email archives.
module NLP.Corpora.Email where

import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT

import Data.MBox (body, Message, parseMBox)

import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

import NLP.Tokenize.Text (tokenize)

-- | Path to the directory containing all the PLUG archives.
plugDataPath :: FilePath
plugDataPath = "./data/corpora/PLUG/"

plugArchiveText :: IO [Text]
plugArchiveText = do
  archive <- fullPlugArchive
  return $ map (LT.toStrict . body) archive

plugArchiveTokens :: IO [[Text]]
plugArchiveTokens = do
  archive <- fullPlugArchive
  return $ map (tokenize . LT.toStrict . body) archive

fullPlugArchive :: IO [Message]
fullPlugArchive = do
  files <- getDirectoryContents plugDataPath
  let archiveFiles = filter (".txt" `isSuffixOf`) files
  contents <- mapM (\f->readF (plugDataPath </> f)) archiveFiles
  return $ concatMap parseMBox contents

readF :: FilePath -> IO LT.Text
readF file = do
  bs <- BS.readFile file
  return $ LT.fromStrict $ TE.decodeLatin1 bs
