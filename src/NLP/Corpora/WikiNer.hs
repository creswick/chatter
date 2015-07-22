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
module NLP.Corpora.WikiNer
  ( parseWikiNer
  , Chunk(..)
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Set (Set)

import Data.Serialize (Serialize)
import Text.Read (readEither)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import GHC.Generics

import NLP.Types.IOB hiding (parseIOB)
import NLP.Types.General (Error, toEitherErr)
import NLP.Types.Tags

import qualified NLP.Corpora.Conll as Conll

parseWikiNer :: Text -> Either Error [[IOBChunk Chunk Conll.Tag]]
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

-- | Tranlsate a WikiNER sentence into a list of IOB-lines, for
-- parsing with `parseIOBLine`
toIOBLines :: Text -> [Text]
toIOBLines sent = map (T.replace "|" " ") (T.words sent)
