{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
module NLP.Extraction.Extractor
  ( parse
  , txtTok
  , posTok
  , Noun(..)
  , simpleNP
  , nps
  )
where

import Data.Text (Text)
import Data.Maybe (catMaybes)
import Text.Parsec.String () -- required for the `Stream [t] Identity t` instance.
import Text.Parsec.Prim (parse, token, Parsec, (<|>), many, try)
-- import Text.Parsec.Combinator ( option )
import Text.Parsec.Pos  (newPos)

import NLP.Types (TaggedSentence, Tag(..))

type Extractor = Parsec TaggedSentence ()

-- | Consume a token with the given POS Tag
posTok :: Tag -> Extractor (Text, Tag)
posTok tag = token showTok posFromTok testTok
  where
    showTok (_,t)       = show t
    posFromTok (_,_)    = newPos "unknown" 0 0
    testTok tok@(_,t) = if tag == t then Just tok else Nothing

-- | Consume a token with the given lexical representation.
txtTok :: Text -> Extractor (Text, Tag)
txtTok txt = token showTok posFromTok testTok
  where
    showTok (t,_)     = show t
    posFromTok (_,_)  = newPos "unknown" 0 0
    testTok tok@(t,_) = if txt == t then Just tok else Nothing

-- | Consume any one non-empty token.
anyToken :: Extractor (Text, Tag)
anyToken = token showTok posFromTok testTok
  where
    showTok (txt,_)     = show txt
    posFromTok (_,_)    = newPos "unknown" 0 0
    testTok tok@(txt,_) | txt == "" = Nothing
                        | otherwise  = Just tok

data Noun = N Text
  deriving (Read, Show, Eq, Ord)

-- | Parse a noun ('nn') that immediately follows an article,
-- returning the Noun
simpleNP :: Extractor Noun
simpleNP = do
  _ <- posTok $ Tag "at"
  (n, _) <- someNN
  return $ N n

someNN :: Extractor (Text, Tag)
someNN = (try (posTok $ Tag "nn")) <|> (posTok $ Tag "nns")

nps :: Extractor [Noun]
nps = do
  mNouns <- many (mnp <|> anyTok)
  return $ catMaybes mNouns

mnp :: Extractor (Maybe Noun)
mnp =  try (do { n <- simpleNP
               ; return $ Just n
               })

anyTok :: Extractor (Maybe Noun)
anyTok = do
  _ <- anyToken
  return Nothing