{-# LANGUAGE OverloadedStrings    #-}
module NLP.Extraction.Examples.ParsecExamples where

import qualified Data.Text as T
import Data.Text (Text)

import Text.Parsec.Prim (parse, (<|>), try)
import Text.Parsec.Pos
import qualified Text.Parsec.Combinator as PC

import NLP.Types
import NLP.Extraction.Parsec

-- grammar = r"""
--   NP: {<DT|JJ|NN.*>+}          # Chunk sequences of DT, JJ, NN
--   PP: {<IN><NP>}               # Chunk prepositions followed by NP
--   VP: {<VB.*><NP|PP|CLAUSE>+$} # Chunk verbs and their arguments
--   CLAUSE: {<NP><VP>}           # Chunk NP, VP
--   """
-- cp = nltk.RegexpParser(grammar)
-- sentence = [("Mary", Tag "NN"), ("saw", Tag "VBD"), ("the", Tag "DT"), ("cat", Tag "NN"), ("sit", Tag "VB"), ("on", Tag "IN"), ("the", Tag "DT"), ("mat", Tag "NN")]

-- | Find a clause in a larger collection of text.
--
-- findClause skips over leading tokens, if needed, to locate a
-- clause.
findClause :: Extractor (Text, Tag)
findClause = followedBy anyToken clause

clause :: Extractor (Text, Tag)
clause = do
  np <- nounPhrase
  vp <- verbPhrase
  return $ chunk [np, vp] $ Tag "clause"

prepPhrase :: Extractor (Text, Tag)
prepPhrase = do
  prep <- posTok $ Tag "IN"
  np <- nounPhrase
  return $ chunk [prep, np] (Tag "p-phr")

nounPhrase :: Extractor (Text, Tag)
nounPhrase = do
  nlist <- PC.many1 (try (posTok $ Tag "NN")
              <|> try (posTok $ Tag "DT")
                  <|> (posTok $ Tag "JJ"))
  let term = T.intercalate " " (map fst nlist)
  return (term, Tag "n-phr")

--  VP: {<VB.*><NP|PP|CLAUSE>+$} # Chunk verbs and their arguments
--  CLAUSE: {<NP><VP>}
verbPhrase :: Extractor (Text, Tag)
verbPhrase = do
  vp <- posPrefix "V"
  obj <- PC.many1 $ ((try clause)
                  <|> (try nounPhrase)
                  <|> prepPhrase)
  return $ chunk (vp:obj) $ Tag "v-phr"


-- | Create a chunked tag from a set of incomming tagged tokens.
chunk :: [(Text, Tag)] -- ^ The incomming tokens to create a chunk from.
      -> Tag           -- ^ The tag for the chunk.
      -> (Text, Tag)
chunk tss tg = (T.unwords (map fst tss), tg)
