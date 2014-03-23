{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
module NLP.Extraction.Extractor where

import Data.Text (Text)
import Text.Parsec.String () -- required for the `Stream [t] Identity t` instance.
import Text.Parsec.Prim (parse, token, Parsec)
import Text.Parsec.Pos  (newPos)

import NLP.Types (TaggedSentence)

type Extractor = Parsec TaggedSentence ()

posTok :: Tag -> Parsec TaggedSentence () Text
posTok tag = token showTok posFromTok testTok
  where
    showTok (_,t)    = show t
    posFromTok (_,_) = newPos "unknown" 0 0
    testTok (txt,t)  = if tag == t then Just txt else Nothing

data Noun = N Text deriving (Read, Show, Eq, Ord)

-- | Parse a noun ('nn') that immediately follows an article, returning the Noun
parseNoun :: Extractor Noun
parseNoun = do
  _ <- posTok $ Tag "at"
  n <- posTok $ Tag "nn"
  return $ N n