{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Parsing for IOB-format corpora.
--
-- Reads in text formatted as:
--
-- <token> <POS tag> <IOB tag>
--
-- Where <IOB tag> is of the format:
--
--  [I-<tag>|B-<tag>|O]
--
-- Since the IOB format is used for various types of information, such
-- as chunks and named-entity classes, the allowable <tag> bits change
-- depending on the corpus.
--
module NLP.Types.IOB where

import Prelude hiding (print)
import Data.Hashable (Hashable(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics

import Text.PrettyPrint.HughesPJClass (Pretty(..))
import Text.PrettyPrint.HughesPJClass (text)


import NLP.Types.General (Error)
import NLP.Types.Annotations
import NLP.Types.Classes
import NLP.Types.TokenizedSentence
import NLP.Types.TaggedSentence
import NLP.Types.ChunkedSentence

import NLP.Tokenize (runTokenizer, whitespace)

-- | Data type to indicate IOB tags for multi-token annotations.
-- The text tag is left un-parsed at this point.
data IOB = B Text -- ^ Beging marker.
         | I Text -- ^ In tag
         | O      -- ^ Not in a tagged thing.
  deriving (Read, Show, Eq, Generic, Ord)

instance Hashable IOB

-- | Literal representation of IOB data.
data IOBTaggedSentence pos =
  IOBTaggedSentence { iobTagSentence :: TaggedSentence pos
                    , iobAnnotations :: [Annotation (TaggedSentence pos) IOB]
                    } deriving (Read, Show, Eq, Generic, Ord)

instance Hashable pos => Hashable (IOBTaggedSentence pos)

instance AnnotatedText (IOBTaggedSentence pos) where
  getText = getText . iobTagSentence

instance Pretty pos => Pretty (IOBTaggedSentence pos) where
  -- TODO: Write this instance.
  pPrint _ts = text "IOBTaggedSentence - instance needs written"

-- | Parse an IOB-tagged corpus into intermediate tagged sentences
-- that are ready for additional parsing into 'ChunkedSentences'
-- or 'NERedSentences'
parse :: POS pos => Text -> Either Error [IOBTaggedSentence pos]
parse input = mapM parseIOB $ parseIOBSentences input

-- | Parse a single sentence from IOB format.
parseIOB :: POS pos => Annotation Text Token -> Either Error (IOBTaggedSentence pos)
parseIOB ann = do
  -- Get the tokens: This should result in a TokenizedSentence with 3*n annotations.
  let rawTokSent = runTokenizer whitespace $ getText ann
      tokSent = rawTokSent {
                  tokAnnotations = map (offsetAnnotations (fromIndex $ startIdx ann)) (tokAnnotations rawTokSent)
                }
  triples <- resolveTriples (tokAnnotations tokSent) []
  let realTokens = [x | (x,_,_) <- triples]
      posTags = [y | (_,y,_) <- triples]
      iobTags = [z | (_,_,z) <- triples]

      realTokSent = TokenizedSentence { tokText = getText ann
                                      , tokAnnotations = realTokens }
      tagSent = TaggedSentence { tagTokSentence = realTokSent
                               , tagAnnotations = zipWith
                                 (\x pos -> Annotation {
                                               startIdx = Index x
                                             , len = 1
                                             , value = pos
                                             , payload = realTokSent }) [1..] posTags
                               }
      iobSent = IOBTaggedSentence { iobTagSentence = tagSent
                                  , iobAnnotations =  zipWith
                                    (\x iob -> Annotation {
                                               startIdx = Index x
                                             , len = 1
                                             , value = iob
                                             , payload = tagSent }) [1..] iobTags
                                  }
  return iobSent

resolveTriples :: POS pos
                => [Annotation Text Token]
                -> [( Annotation Text Token, pos, IOB)]
                -> Either Error [( Annotation Text Token, pos, IOB)]
resolveTriples                       [] acc = Right acc
resolveTriples (tok:rawPos:rawIOB:rest) acc = do
  pos <- parsePOS $ getText rawPos
  iob <- iobBuilder $ getText rawIOB
  recurse <- resolveTriples rest acc
  return ((tok, pos, iob):recurse)
resolveTriples cruft _ = Left ("Could not parse IOB tripple from: \"" <> (T.pack $ show cruft) <> "\"")


-- | Shift an annotation by a given distance.
offsetAnnotations :: Int -> Annotation a b -> Annotation a b
offsetAnnotations offset ann =
  ann { startIdx = Index (offset + (fromIndex $ startIdx ann))
      }

-- | Parse IOB text into sentences, using the Annotations
-- structure to store *one* sentence per token.
--
parseIOBSentences :: Text -> [Annotation Text Token]
parseIOBSentences input = reverse $ addLast $ T.foldl' fn emptyAcc input
  where
    emptyAcc :: ( Int -- Index
                , Char -- Last char
                , [Char] -- Accumulated tokens.
                , [Annotation Text Token] -- Accumulated annotations
                )
    emptyAcc = (0, '\n', [], [])

    addLast :: (Int, Char, [Char], [Annotation Text Token]) -> [Annotation Text Token]
    addLast (idx, _, _, acc) = (mkAnnotation idx (T.unpack $ T.drop idx input)):acc

    fn :: (Int, Char, [Char], [Annotation Text Token])
       -> Char
       -> (Int, Char, [Char], [Annotation Text Token])
    fn (idx, lChar, tok, acc) char | char == '\n' && lChar == '\n' =
                                     case tok of -- end of a token; if we have characters, create new one.
                                       [] -> (idx + 1, char, [], acc)
                                       cs -> (idx + 1, char, [], (mkAnnotation (idx - length cs) $ reverse tok):acc)
                                   | otherwise = (idx + 1, char, char:tok, acc)

    mkAnnotation :: Int -> [Char] -> Annotation Text Token
    mkAnnotation sIdx tok = Annotation { startIdx = Index sIdx
                                       , len = length tok
                                       , value = Token $ T.pack tok
                                       , payload = input
                                       }

iobBuilder :: Text -> Either Error IOB
iobBuilder iobTxt | "I-" `T.isPrefixOf` iobTxt = Right (I $ T.drop 2 iobTxt)
                  | "B-" `T.isPrefixOf` iobTxt = Right (B $ T.drop 2 iobTxt)
                  | "O"  `T.isPrefixOf` iobTxt = Right O
                  | otherwise                  = Left ("Could not parse IOB text: \""<>iobTxt<>"\"")

-- -- | Turn an IOB result into a tree.
-- toChunkTree :: (ChunkTag c, Tag t) => [IOBChunk c t] -> ChunkedSentence c t
-- toChunkTree chunks = ChunkedSent $ toChunkOr chunks

--   where
--     toChunkOr :: (ChunkTag c, Tag t) => [IOBChunk c t] -> [ChunkOr c t]
--     toChunkOr [] = []
--     toChunkOr ((OChunk pos):rest)       = POS_CN pos : toChunkOr rest
--     toChunkOr (ch:rest) = case ch of
--       (BChunk pos chunk) -> (Chunk_CN (Chunk chunk children)) : toChunkOr theTail
--       (IChunk pos chunk) -> (Chunk_CN (Chunk chunk children)) : toChunkOr theTail
--       where
--         (ichunks, theTail) = span isIChunk rest

--         toPOScn (IChunk pos _) = Just $ POS_CN pos
--         toPOScn _              = Nothing

--     --    children :: [ChunkOr c t]
--         children = mapMaybe toPOScn ichunks

--         isIChunk (IChunk _ _) = True
--         isIChunk _            = False

-- -- | Parse an IOB-encoded corpus.
-- parseIOB :: (ChunkTag chunk, Tag tag) => Text -> Either Error [[IOBChunk chunk tag]]
-- parseIOB corpora =
--   let sentences = getSentences corpora
--   in sequence $ map parseSentence sentences

-- parseSentence :: (ChunkTag chunk, Tag tag) => [Text] -> Either Error [IOBChunk chunk tag]
-- parseSentence input = sequence (map parseIOBLine input)



-- -- | Just split a body of text into lines, and then into "paragraphs".
-- -- Each resulting sub list is separated by empty lines in the original text.
-- --
-- -- e.g.;
-- -- > > getSentences "He\njumped\n.\n\nShe\njumped\n."
-- -- > [["He", "jumped", "."], ["She","jumped", "."]]
-- --
-- getSentences :: Text -> [[Text]]
-- getSentences corpora =
--   let theLines = map T.strip $ T.lines corpora

--       sentences :: [Text] -> [[Text]]
--       sentences []      = []
--       sentences ("":xs) = sentences xs
--       sentences input   = let (sent, rest) = break (== T.empty) input
--                           in (sent:sentences rest)

--   in sentences theLines
