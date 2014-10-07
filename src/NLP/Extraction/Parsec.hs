{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This is a very simple wrapper around Parsec for writing
-- Information Extraction patterns.
--
-- Because the particular tags/tokens to parse depends on the training
-- corpus (for POS tagging) and the domain, this module only provides
-- basic extractors.  You can, for example, create an extractor to
-- find noun phrases by combining the components provided here:
--
-- @
--   nounPhrase :: Extractor (Text, Tag)
--   nounPhrase = do
--     nlist <- many1 (try (posTok $ Tag \"NN\")
--                 \<|\> try (posTok $ Tag \"DT\")
--                     \<|\> (posTok $ Tag \"JJ\"))
--     let term = T.intercalate " " (map fst nlist)
--     return (term, Tag "n-phr")
-- @
module NLP.Extraction.Parsec

where

-- See this SO q/a for some possibly useful combinators:
--  http://stackoverflow.com/questions/2473615/parsec-3-1-0-with-custom-token-datatype

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec.String () -- required for the `Stream [t] Identity t` instance.
import Text.Parsec.Prim (lookAhead, token, Parsec, try, Stream(..))
import qualified Text.Parsec.Combinator as PC
import Text.Parsec.Pos  (newPos)

import NLP.Types (TaggedSentence(..), Tag(..), CaseSensitive(..))

instance (Monad m, Tag t) => Stream (TaggedSentence t) m (Text, t) where
  uncons (TS ts) = do
    mRes <- uncons ts
    case mRes of
      Nothing           -> return $ Nothing
      Just (mTok, rest) -> return $ Just (mTok, TS rest)
  {-# INLINE uncons #-}

-- | A Parsec parser.
--
-- Example usage:
--
-- @
-- > set -XOverloadedStrings
-- > import Text.Parsec.Prim
-- > parse myExtractor "interactive repl" someTaggedSentence
-- @
type Extractor t = Parsec (TaggedSentence t) ()

-- | Consume a token with the given POS Tag
posTok :: Tag t => t -> Extractor t (Text, t)
posTok tag = token showTok posFromTok testTok
  where
    showTok (_,t)       = show t
    posFromTok (_,_)    = newPos "unknown" 0 0
    testTok tok@(_,t) = if tag == t then Just tok else Nothing

-- | Consume a token with the specified POS prefix.
--
-- @
-- > parse (posPrefix "n") "ghci" [("Bob", Tag "np")]
-- Right [("Bob", Tag "np")]
-- @
posPrefix :: Tag t => Text -> Extractor t (Text, t)
posPrefix str = token showTok posFromTok testTok
  where
    showTok (_,t)     = show t
    posFromTok (_,_)  = newPos "unknown" 0 0
    testTok tok@(_,t) = if str `T.isPrefixOf` (tagTerm t) then Just tok else Nothing

-- | Text equality matching with optional case sensitivity.
matches :: CaseSensitive -> Text -> Text -> Bool
matches Sensitive   x y = x == y
matches Insensitive x y = (T.toLower x) == (T.toLower y)

-- | Consume a token with the given lexical representation.
txtTok :: Tag t => CaseSensitive -> Text -> Extractor t (Text, t)
txtTok sensitive txt = token showTok posFromTok testTok
  where
    showTok (t,_)     = show t
    posFromTok (_,_)  = newPos "unknown" 0 0
    testTok tok@(t,_) | matches sensitive txt t = Just tok
                      | otherwise               = Nothing

-- | Consume any one non-empty token.
anyToken :: Tag t => Extractor t (Text, t)
anyToken = token showTok posFromTok testTok
  where
    showTok (txt,_)     = show txt
    posFromTok (_,_)    = newPos "unknown" 0 0
    testTok tok@(txt,_) | txt == "" = Nothing
                        | otherwise  = Just tok

oneOf :: Tag t => CaseSensitive -> [Text] -> Extractor t (Text, t)
oneOf sensitive terms = PC.choice (map (\t -> try (txtTok sensitive t)) terms)

-- | Skips any number of fill tokens, ending with the end parser, and
-- returning the last parsed result.
--
-- This is useful when you know what you're looking for and (for
-- instance) don't care what comes first.
followedBy :: Tag t => Extractor t b -> Extractor t a -> Extractor t a
followedBy fill end = do
  _ <- PC.manyTill fill (lookAhead end)
  end
