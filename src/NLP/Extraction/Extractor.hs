{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
module NLP.Extraction.Extractor where

import Data.Text (Text)
import Text.Parsec.String () -- required for the `Stream [t] Identity t` instance.
import Text.Parsec.Prim
import Text.Parsec.Pos

import Data.Functor.Identity (Identity)

import NLP.Types

type Extractor = forall u. Parsec TaggedSentence u

posTok :: forall u.
          (Stream TaggedSentence Identity (Text, Tag)) =>
          Tag -> Parsec TaggedSentence u Text
posTok tag = token showTok posFromTok testTok
  where
    showTok (_,t)    = show t
    posFromTok (_,_) = newPos "unknown" 0 0
    testTok (txt,t)  = if tag == t then Just txt else Nothing

data Noun = N Text deriving (Read, Show, Eq, Ord)

-- | Parse a noun ('nn') that immediately follows an article, returning the Noun
parseNoun ::  forall u. Parsec TaggedSentence u Noun
parseNoun = do
  posTok $ Tag "at"
  n <- posTok $ Tag "nn"
  return $ N n

-- Extractor.hs:38:3:
--     No instance for (Stream [(Text, Tag)] Identity (Text, Tag))
--       arising from a use of `posTok'
--     Possible fix:
--       add an instance declaration for
--       (Stream [(Text, Tag)] Identity (Text, Tag))
--     In the expression: posTok
--     In a stmt of a 'do' block: posTok $ Tag "at"
--     In the expression:
--       do { posTok $ Tag "at";
--            n <- posTok $ Tag "nn";
--            return $ N n }