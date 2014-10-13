{-# LANGUAGE OverloadedStrings #-}
module NLP.Types.IOB where

import Prelude hiding (print)
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import qualified Data.Text as T

import Test.QuickCheck (Arbitrary(..), elements)
import Test.QuickCheck.Instances ()

import NLP.Types.Tags
import NLP.Types.Tree
import NLP.Types.General (Error)

-- | Data type to indicate IOB tags for chunking
data IOBChunk chunk tag = BChunk (POS tag) chunk -- ^ Beging marker.
                        | IChunk (POS tag) chunk -- ^ In chunk tag
                        | OChunk (POS tag) -- ^ Not in a chunk.
  deriving (Read, Show, Eq)

getPOS :: (ChunkTag c, Tag t) => IOBChunk c t -> POS t
getPOS (BChunk pos _) = pos
getPOS (IChunk pos _) = pos
getPOS (OChunk pos)   = pos

instance (ChunkTag c, Arbitrary c, Arbitrary t, Tag t) => Arbitrary (IOBChunk c t) where
  arbitrary = elements =<< do
                ic <- IChunk <$> arbitrary <*> arbitrary
                bc <- BChunk <$> arbitrary <*> arbitrary
                oc <- OChunk <$> arbitrary
                return [ic, bc, oc]

toTaggedSentence :: (ChunkTag c, Tag t) => [IOBChunk c t] -> TaggedSentence t
toTaggedSentence iobChunks = TaggedSent $ map getPOS iobChunks


-- | Parse an IOB-chunk encoded line of text.
--
-- Assumes that the line has three space-delimeted entries, in the format:
-- > token POSTag IOBChunk
-- For example:
-- > > parseIOBLine "We PRP B-NP" :: IOBChunk B.Chunk B.Tag
-- > BChunk (POS B.PRP (Token "We")) B.C_NP
--
parseIOBLine :: (ChunkTag chunk, Tag tag) => Text -> Either Error (IOBChunk chunk tag)
parseIOBLine txt =
  let (tokTxt:tagTxt:iobTxt:_) = T.words txt
      token = Token tokTxt
      tag   = POS (parseTag tagTxt) token
  in iobBuilder iobTxt tag

iobBuilder :: (ChunkTag c, Tag t) => Text -> (POS t -> Either Error (IOBChunk c t))
iobBuilder iobTxt | "I-" `T.isPrefixOf` iobTxt = \tag -> (IChunk tag) <$> chunk
                  | "B-" `T.isPrefixOf` iobTxt = \tag -> (BChunk tag) <$> chunk
                  | otherwise                  = \tag -> Right (OChunk tag)
  where
    chunk = parseChunk (T.drop 2 iobTxt)

-- | Parse an IOB-encoded corpus.
parseIOB :: (ChunkTag chunk, Tag tag) => Text -> Either Error [[IOBChunk chunk tag]]
parseIOB corpora =
  let sentences = getSentences corpora
  in sequence $ map parseSentence sentences

parseSentence :: (ChunkTag chunk, Tag tag) => [Text] -> Either Error [IOBChunk chunk tag]
parseSentence input = sequence (map parseIOBLine input)

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
