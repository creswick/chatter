{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | A parser for the Wiki NER work presented in:
--
-- @Article{nothman2012:artint:wikiner,
--   author = {Joel Nothman and Nicky Ringland and Will Radford and Tara Murphy and James R. Curran},
--   title = {Learning multilingual named entity recognition from {Wikipedia}},
--   journal = {Artificial Intelligence},
--   publisher = {Elsevier},
--   volume = {194},
--   pages = {151--175},
--   year = {2012},
--   doi = {10.1016/j.artint.2012.03.006},
--   url = {http://dx.doi.org/10.1016/j.artint.2012.03.006}
-- }
--
-- And provided here: http://schwa.org/projects/resources/wiki/Wikiner
--
-- The format does not appear to be documented, but it looks like:
--
--  * One sentence per line.
--
--  * Tagged tokens are separated by spaces
--
--  * Items in a tagged token are separated by vertical bars ('|')
--
--  * Each line of `n` text tokens contains 3*n items, starting with a
--  text token, a POS tag, then a IOB tag with one of the NER classes
--
-- For example, the sentence:
--   The Oxford Companion to Philosophy says, "there is no single defining position that all anarchists hold, and those considered anarchists at best sharae a certain family resemblance."
--
-- Is rendered as:
--  The|DT|I-MISC Oxford|NNP|I-MISC Companion|NNP|I-MISC to|TO|I-MISC Philosophy|NNP|I-MISC says|VBZ|O ,|,|O "|LQU|O there|EX|O is|VBZ|O no|DT|O single|JJ|O defining|VBG|O position|NN|O that|IN|O all|DT|O anarchists|NNS|O hold|VBP|O ,|,|O and|CC|O those|DT|O considered|VBN|O anarchists|NNS|O at|IN|O best|JJS|O share|NN|O a|DT|O certain|JJ|O family|NN|O resemblance|NN|O .|.|O "|RQU|O
--
--
--  This module also provides a trained model for NER via the averaged
--  perceptron chunker.  This actually kindof works, which is a bit
--  amazing.  For example:
--
-- > import NLP.Corpora.WikiNer
-- > import NLP.POS
-- > import NLP.Chunk
-- > tgr <- defaultTagger
-- > chk <- wikiNerChunker
-- > chunkText tgr chk "Real World Haskell is a book created by Don Stewart, Bryan O'Sullivan, and Jon Goerzen."
-- > "[ORG Real/NNP] [MISC World/NNP] [PER Haskell/NNP] is/VBZ a/DT book/NN created/VBN by/IN [PER Don/NNP Stewart/NNP] ,/, [PER Bryan/NNP O'Sullivan/NNP] ,/, and/CC [PER Jon/NNP Goerzen/NNP] ./."
--
--
module NLP.Corpora.WikiNer
  ( parseWikiNer
  , trainChunker
  , wikiNerChunker
  , Chunk(..)
  )
where

import           Data.Text                      (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Serialize                 (Serialize)
import           GHC.Generics
import           System.FilePath                ((</>))
import           Text.Read                      (readEither)
import           Test.QuickCheck.Arbitrary      (Arbitrary(..))
import           Test.QuickCheck.Gen            (elements)


import           NLP.Chunk                      (train, loadChunker)
import           NLP.Chunk.AvgPerceptronChunker (Chunker(..), mkChunker)
import qualified NLP.Corpora.Conll as C
import           NLP.ML.AvgPerceptron           ( emptyPerceptron )
import           NLP.Types.IOB hiding           (parseIOB)
import           NLP.Types.General              (Error, toEitherErr)
import           NLP.Types.Tags

import           Paths_chatter

parseWikiNer :: Text -> Either Error [[IOBChunk Chunk C.Tag]]
parseWikiNer = parseIOB

-- | Convert wikiNer format to basic IOB (one token perline, space
-- separated tags, and a blank line between each sentence)
parseIOB :: (ChunkTag chunk, Tag tag) => Text -> Either Error [[IOBChunk chunk tag]]
parseIOB input = sequence $ map (parseSentence . toIOBLines) (filter (/="") $ T.lines input)

-- | Different classes of Named Entity used in the WikiNER data set.
data Chunk = LOC
           | MISC
           | ORG
           | PER
           | C_O -- ^ "out" not a chunk.
             deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)


instance Arbitrary Chunk where
  arbitrary = elements [minBound ..]

instance Serialize Chunk

instance ChunkTag Chunk where
  fromChunk = T.pack . show
  parseChunk txt = toEitherErr $ readEither (T.unpack txt)
  notChunk = C_O

wikiNerChunker :: IO (Chunker Chunk C.Tag)
wikiNerChunker = do
  dir <- getDataDir
  loadChunker (dir </> "data" </> "models" </> "wikiner.ner.model.gz")

-- | Tranlsate a WikiNER sentence into a list of IOB-lines, for
-- parsing with `parseIOBLine`
toIOBLines :: Text -> [Text]
toIOBLines sent = map (T.replace "|" " ") (T.words sent)

-- | Train a chunker on a provided corpus.
trainChunker :: [FilePath] -> IO (Chunker Chunk C.Tag)
trainChunker corpora = do
  content <- mapM T.readFile corpora

  let trainingText = T.intercalate "\n" content

      eiobs = parseWikiNer trainingText

      chunker :: Chunker Chunk C.Tag
      chunker = mkChunker emptyPerceptron

  case eiobs of
    Left   err -> do
      T.putStrLn err
      error (T.unpack err)
    Right iobs -> do
      print (take 1 iobs)
      let chunkSents = map toChunkTree iobs
      train chunker chunkSents

