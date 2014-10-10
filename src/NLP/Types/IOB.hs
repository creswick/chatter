{-# LANGUAGE OverloadedStrings #-}
module NLP.Types.IOB where

import Prelude hiding (print)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Text as T

import Test.QuickCheck (Arbitrary(..), listOf, elements, NonEmptyList(..))
import Test.QuickCheck.Instances ()

import NLP.Types.Tags
import NLP.Types.Tree
import NLP.Types.General (Error(..))

-- | Data type to indicate IOB tags for chunking
data IOBChunk chunk tag = BChunk (POS tag) chunk -- ^ Beging marker.
                        | IChunk (POS tag) chunk -- ^ In chunk tag
                        | OChunk (POS tag) -- ^ Not in a chunk.
  deriving (Read, Show, Eq)

instance (ChunkTag c, Arbitrary c, Arbitrary t, Tag t) => Arbitrary (IOBChunk c t) where
  arbitrary = elements =<< do
                ic <- IChunk <$> arbitrary <*> arbitrary
                bc <- BChunk <$> arbitrary <*> arbitrary
                oc <- OChunk <$> arbitrary
                return [ic, bc, oc]

-- | Parse an IOB-chunk encoded line of text.
--
-- Assumes that the line has three space-delimeted entries, in the format:
-- > token POSTag IOBChunk
-- For example:
-- > > parseIOBChunk "We PRP B-NP" :: IOBChunk B.Chunk B.Tag
-- > BChunk (POS B.PRP (Token "We")) B.C_NP
--
parseIOBChunk :: (ChunkTag chunk, Tag tag) => Text -> Either Error (IOBChunk chunk tag)
parseIOBChunk txt =
  let (tokTxt:tagTxt:iobTxt:_) = T.words txt
      token = Token tokTxt
      tag   = POS (parseTag tagTxt) token
      chunk = parseChunk (T.drop 2 iobTxt)
      iobChunk | "I-" `T.isPrefixOf` iobTxt = (IChunk tag) <$> chunk
               | "B-" `T.isPrefixOf` iobTxt = (BChunk tag) <$> chunk
               | otherwise                  = Right (OChunk tag)
  in iobChunk

-- | Parse an IOB-encoded corpus.
parseIOB :: (ChunkTag chunk, Tag tag) => Text -> Either Error [[IOBChunk chunk tag]]
parseIOB corpora =
  let sentences = getSentences corpora
  in sequence $ map parseSentence sentences

parseSentence :: (ChunkTag chunk, Tag tag) => [Text] -> Either Error [IOBChunk chunk tag]
parseSentence input = sequence (map parseIOBChunk input)

-- | Just split a body of text into lines, and then into "paragraphs".
-- Each resulting sub list is separated by empty lines in the original text.
--
-- e.g.;
-- > > getSentences "He\njumped\n.\n\nShe\njumped\n."
-- > [["He", "jumped", "."], ["She","jumped", "."]]
--
getSentences :: Text -> [[Text]]
getSentences corpora =
  let theLines = map T.strip $ T.lines corpora

      sentences :: [Text] -> [[Text]]
      sentences []      = []
      sentences ("":xs) = sentences xs
      sentences input   = let (sent, rest) = break (== T.empty) input
                          in (sent:sentences rest)

  in sentences theLines
