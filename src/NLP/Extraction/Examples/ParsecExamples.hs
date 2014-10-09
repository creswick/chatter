{-# LANGUAGE OverloadedStrings    #-}
module NLP.Extraction.Examples.ParsecExamples where

import qualified Data.Text as T

import Text.Parsec.Prim ( (<|>), try)
import qualified Text.Parsec.Combinator as PC

import NLP.Types
import NLP.Extraction.Parsec

import qualified NLP.Corpora.Brown as B

-- grammar = r"""
--   NP: {<DT|JJ|NN.*>+}          # Chunk sequences of DT, JJ, NN
--   PP: {<IN><NP>}               # Chunk prepositions followed by NP
--   VP: {<VB.*><NP|PP|CLAUSE>+$} # Chunk verbs and their arguments
--   CLAUSE: {<NP><VP>}           # Chunk NP, VP
--   """
-- cp = nltk.RegexpParser(grammar)
-- sentence = [("Mary", Tag "NN"), ("saw", Tag "VBD"), ("the", Tag "DT"), ("cat", Tag "NN"), ("sit", Tag "VB"), ("on", Tag "IN"), ("the", Tag "DT"), ("mat", Tag "NN")]
-- sentence = [TaggedSent [POS NP (Token "Mary"),POS VBD (Token "saw"),POS DT (Token "the"),POS NN (Token "cat"),POS VB (Token "sit"),POS IN (Token "on"),POS DT (Token "the"),POS NN (Token "mat"),POS Term (Token ".")]]
-- | Find a clause in a larger collection of text.
--
-- findClause skips over leading tokens, if needed, to locate a
-- clause.
findClause :: Extractor B.Tag (ChunkOr B.Chunk B.Tag)
findClause = followedBy anyToken clause

clause :: Extractor B.Tag (ChunkOr B.Chunk B.Tag)
clause = do
  np <- nounPhrase
  vp <- verbPhrase
  return $ mkChunk B.C_CL [np, vp]

prepPhrase :: Extractor B.Tag (ChunkOr B.Chunk B.Tag)
prepPhrase = do
  prep <- posTok B.IN
  np <- nounPhrase
  return $ mkChunk B.C_PP [POS_CN prep, np]

nounPhrase :: Extractor B.Tag (ChunkOr B.Chunk B.Tag)
nounPhrase = do
  nlist <- PC.many1 (try (posTok $ B.NN)
              <|> try (posTok $ B.DT)
                      <|> try (posTok $ B.AT) -- tagger often gets 'the' wrong.
                              <|> (posTok $ B.JJ))
  return (mkChunk B.C_NP $ map POS_CN nlist)

--  VP: {<VB.*><NP|PP|CLAUSE>+$} # Chunk verbs and their arguments
--  CLAUSE: {<NP><VP>}
verbPhrase :: Extractor B.Tag (ChunkOr B.Chunk B.Tag)
verbPhrase = do
  vp <- posPrefix "V"
  obj <- PC.many1 $ ((try clause)
                  <|> (try nounPhrase)
                  <|> prepPhrase)
  return $ mkChunk B.C_VP $ ((POS_CN vp):obj)


-- -- | Create a chunked tag from a set of incomming tagged tokens.
-- chunk :: [(POS B.Tag)] -- ^ The incomming tokens to create a chunk from.
--       -> B.Tag           -- ^ The tag for the chunk.
--       -> (POS B.Tag)
-- chunk tss tg = POS tg $ Token (T.unwords (map showPOS tss))
