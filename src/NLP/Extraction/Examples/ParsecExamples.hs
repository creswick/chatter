{-# LANGUAGE OverloadedStrings    #-}
-- | Example parsing with Parsec.
--
-- This example shows how the following grammar, from NLTK, can be
-- implemented in Chatter, using Parsec-based Information Extraction
-- patterns:
--
-- > grammar = r"""
-- >  NP: {<DT|JJ|NN.*>+}          # Chunk sequences of DT, JJ, NN
-- >  PP: {<IN><NP>}               # Chunk prepositions followed by NP
-- >  VP: {<VB.*><NP|PP|CLAUSE>+$} # Chunk verbs and their arguments
-- >  CLAUSE: {<NP><VP>}           # Chunk NP, VP
-- >  """
--
-- > > import NLP.Extraction.Examples.ParsecExamples
-- > > import Text.Parsec.Prim
-- > > tgr <- defaultTagger
-- > > map (parse findClause "interactive") $ tag tgr "Mary saw the cat sit on the mat."
-- > [Right (Chunk_CN (Chunk C_CL [Chunk_CN (Chunk C_NP [POS_CN (POS AT (Token "the")),POS_CN (POS NN (Token "cat"))]),Chunk_CN (Chunk C_VP [POS_CN (POS VB (Token "sit")),Chunk_CN (Chunk C_PP [POS_CN (POS IN (Token "on")),Chunk_CN (Chunk C_NP [POS_CN (POS AT (Token "the")),POS_CN (POS NN (Token "mat"))])])])]))]
--
module NLP.Extraction.Examples.ParsecExamples where

import Text.Parsec.Prim ( (<|>), try)
import qualified Text.Parsec.Combinator as PC

import NLP.Types
import NLP.Extraction.Parsec

import qualified NLP.Corpora.Brown as B



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
