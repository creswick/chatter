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
findClause :: Extractor RawTag (Text, RawTag)
findClause = followedBy anyToken clause

clause :: Extractor RawTag (Text, RawTag)
clause = do
  np <- nounPhrase
  vp <- verbPhrase
  return $ chunk [np, vp] $ RawTag "clause"

prepPhrase :: Extractor RawTag (Text, RawTag)
prepPhrase = do
  prep <- posTok $ RawTag "IN"
  np <- nounPhrase
  return $ chunk [prep, np] (RawTag "p-phr")

nounPhrase :: Extractor RawTag (Text, RawTag)
nounPhrase = do
  nlist <- PC.many1 (try (posTok $ RawTag "NN")
              <|> try (posTok $ RawTag "DT")
                  <|> (posTok $ RawTag "JJ"))
  let term = T.intercalate " " (map fst nlist)
  return (term, RawTag "n-phr")

--  VP: {<VB.*><NP|PP|CLAUSE>+$} # Chunk verbs and their arguments
--  CLAUSE: {<NP><VP>}
verbPhrase :: Extractor RawTag (Text, RawTag)
verbPhrase = do
  vp <- posPrefix "V"
  obj <- PC.many1 $ ((try clause)
                  <|> (try nounPhrase)
                  <|> prepPhrase)
  return $ chunk (vp:obj) $ RawTag "v-phr"


-- | Create a chunked tag from a set of incomming tagged tokens.
chunk :: [(Text, RawTag)] -- ^ The incomming tokens to create a chunk from.
      -> RawTag           -- ^ The tag for the chunk.
      -> (Text, RawTag)
chunk tss tg = (T.unwords (map fst tss), tg)
