{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
module NLP.Extraction.Parsec

where

-- See this SO q/a for some possibly useful combinators:
--  http://stackoverflow.com/questions/2473615/parsec-3-1-0-with-custom-token-datatype

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
-- import Text.Parsec.String () -- required for the `Stream [t] Identity t` instance.
import Text.Parsec.Prim (lookAhead, token, Parsec, try)
import qualified Text.Parsec.Combinator as PC
import Text.Parsec.Pos  (newPos)

import NLP.Types (TaggedSentence, Tag(..), CaseSensitive(..))

-- | A Parsec parser.
--
-- Example usage:
-- > set -XOverloadedStrings
-- > import Text.Parsec.Prim
-- > parse myExtractor "interactive repl" someTaggedSentence
--
type Extractor = Parsec TaggedSentence ()

-- | Consume a token with the given POS Tag
posTok :: Tag -> Extractor (Text, Tag)
posTok tag = token showTok posFromTok testTok
  where
    showTok (_,t)       = show t
    posFromTok (_,_)    = newPos "unknown" 0 0
    testTok tok@(_,t) = if tag == t then Just tok else Nothing

-- | Consume a token with the specified POS prefix.
--
-- > parse (posPrefix "n") "ghci" [("Bob", Tag "np")]
-- Right [("Bob", Tag "np")]
--
posPrefix :: Text -> Extractor (Text, Tag)
posPrefix str = token showTok posFromTok testTok
  where
    showTok (_,t)       = show t
    posFromTok (_,_)    = newPos "unknown" 0 0
    testTok tok@(_,Tag t) = if str `T.isPrefixOf` t then Just tok else Nothing

-- | Text equality matching with optional case sensitivity.
matches :: CaseSensitive -> Text -> Text -> Bool
matches Sensitive   x y = x == y
matches Insensitive x y = (T.toLower x) == (T.toLower y)

-- | Consume a token with the given lexical representation.
txtTok :: CaseSensitive -> Text -> Extractor (Text, Tag)
txtTok sensitive txt = token showTok posFromTok testTok
  where
    showTok (t,_)     = show t
    posFromTok (_,_)  = newPos "unknown" 0 0
    testTok tok@(t,_) | matches sensitive txt t = Just tok
                      | otherwise               = Nothing

-- | Consume any one non-empty token.
anyToken :: Extractor (Text, Tag)
anyToken = token showTok posFromTok testTok
  where
    showTok (txt,_)     = show txt
    posFromTok (_,_)    = newPos "unknown" 0 0
    testTok tok@(txt,_) | txt == "" = Nothing
                        | otherwise  = Just tok

oneOf :: CaseSensitive -> [Text] -> Extractor (Text, Tag)
oneOf sensitive terms = PC.choice (map (\t -> try (txtTok sensitive t)) terms)

-- | Skips any number of fill tokens, ending with the end parser, and
-- returning the last parsed result.
--
-- This is useful when you know what you're looking for and (for
-- instance) don't care what comes first.
followedBy :: Extractor b -> Extractor a -> Extractor a
followedBy fill end = do
  _ <- PC.manyTill fill (lookAhead end)
  end