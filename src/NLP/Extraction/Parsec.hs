{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec.String () -- required for the `Stream [t] Identity t` instance.
import Text.Parsec.Prim (lookAhead, tokenPrim, token, Parsec, try, Stream(..), ParsecT)
import Text.Parsec.Pos (incSourceColumn)
import qualified Text.Parsec.Combinator as PC
import qualified Text.Parsec.Prim as PC
import qualified Text.Parsec as PC
import Text.Parsec.Pos  (newPos)

import NLP.Types

instance (Monad m)
         => Stream TokenizedSentence m (Annotation Text Token) where
  uncons (TokenizedSentence   _       []) = return Nothing
  uncons (TokenizedSentence txt (a:anns)) = return $ Just (a, TokenizedSentence txt anns)
  {-# INLINE uncons #-}

instance (Monad m, POS pos)
         => Stream (TaggedSentence pos) m (Annotation TokenizedSentence pos) where
  uncons (TaggedSentence   _       []) = return Nothing
  uncons (TaggedSentence txt (a:anns)) = return $ Just (a, TaggedSentence txt anns)
  {-# INLINE uncons #-}

instance (Monad m, Chunk chunk, POS pos)
         => Stream (ChunkedSentence pos chunk) m (Annotation (TaggedSentence pos) chunk) where
  uncons (ChunkedSentence   _       []) = return Nothing
  uncons (ChunkedSentence txt (a:anns)) = return $ Just (a, ChunkedSentence txt anns)
  {-# INLINE uncons #-}

instance (Monad m, Chunk chunk, POS pos)
         => Stream (ChunkedSentence pos chunk) m (Annotation TokenizedSentence pos) where
  uncons (ChunkedSentence (TaggedSentence _ [])         _) = return Nothing
  -- this is tricky.  If the tagged sentence annotation is not used by
  -- a chunk, then we just return the list of chunk annotations.
  -- However, if the tagged annotation is used by a chunk, then
  -- consuming that invalidates the chunk (the chunk /probably/
  -- doesn't apply to a portion of the text) so we need to discard the
  -- next chunk annotation.
  uncons (ChunkedSentence (TaggedSentence txt (a:anns)) (cann:canns)) = return $
    if (a `coveredby` cann)
       then Just (a, ChunkedSentence (TaggedSentence txt anns) canns)
       else Just (a, ChunkedSentence (TaggedSentence txt anns) (cann:canns))
  {-# INLINE uncons #-}

-- | Determine if the first annotation is included in the second annotation.
coveredby :: Annotation payload1 tag1 -> Annotation payload2 tag2 -> Bool
coveredby = undefined

instance (Monad m, Chunk c, POS pos)
         => Stream (ChunkedSentence pos c) m (Annotation Text Token) where
  uncons (ChunkedSentence   _       []) = return Nothing
  uncons (ChunkedSentence txt (a:anns)) = return $ Just (a, ChunkedSentence txt anns)
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
type Extractor tag chunk = Parsec (ChunkedSentence tag chunk) (ChunkedSentence tag chunk)

doParse :: (POS tag, Chunk chunk) => Extractor tag chunk a -> (ChunkedSentence tag chunk) -> Either PC.ParseError a
doParse parser input = PC.runParser parser input "parse input" input

fullInput :: (POS tag, Chunk chunk) => Extractor tag chunk (ChunkedSentence tag chunk)
fullInput = PC.getState

oneOfT :: (Eq t, Show t, Stream s m t) => [t] -> ParsecT s u m t
oneOfT ts = satisfy (`elem` ts)

noneOfT :: (Eq t, Show t, Stream s m t) => [t] -> ParsecT s u m t
noneOfT ts = satisfy (not . (`elem` ts))

-- | Consume any one token and advance the column count by one.
anyToken :: (Show t, Stream s m t) => ParsecT s u m t
anyToken = satisfy (const True)

satisfy :: (Show t, Stream s m t) => (t -> Bool) -> ParsecT s u m t
satisfy p = tokenPrim showTok nextPos testTok
    where
      showTok t         = show t
      testTok t         = if p t then Just t else Nothing
      nextPos pos _t _s = incSourceColumn pos 1

eof :: (Stream s m t, Show t) => ParsecT s u m ()
eof                 = PC.notFollowedBy anyToken PC.<?> "end of input"

-- | Consume one token that matches the supplied token.
matchToken :: (Show tag) => Token -> Extractor tag chunk (Annotation (TaggedSentence tag) chunk)
matchToken tok = satisfy (\x -> value x == tok)

-- | Matches a sequence of tokens.
matchTokens :: [Token] -> Extractor tag chunk [Annotation (TaggedSentence tag) Token]
matchTokens toks = mapM matchToken toks

-- | Find all instances of the specified sequence of tokens.
--
-- Each sublist in the result is an instance of a match.
findAll :: (Show tag, POS tag, Chunk chunk) => [Token] -> Extractor tag chunk [[Annotation (TaggedSentence tag) chunk]]
findAll toks = do
  res <- PC.many $ (PC.try $ followedBy anyToken (matchTokens toks))  -- TODO remove that PC.try?
  _ <- PC.manyTill anyToken eof
  return res

findNeedles :: [[Token]] -> Extractor tag chunk [[Annotation (TaggedSentence tag) chunk]]
findNeedles items =  do
  let isInteresting = PC.choice $ map (PC.try . matchTokens) items
  res <- PC.many $ (PC.try $ followedBy anyToken isInteresting) -- TODO remove that PC.try?
  _ <- PC.manyTill anyToken PC.eof
  return res

-- | Given a lookup table with multi-token items and their corresponding annotations,
-- Find the things, and annotate them all accordingly.
annotateAllTokens :: [([Token], ann)] -> Extractor tag chunk [Annotation TokenizedSentence ann]
annotateAllTokens toks = do
  let isInteresting = PC.choice $ map (PC.try . annotateToken) toks
  res <- PC.many $ (PC.try $ followedBy anyToken isInteresting) -- TODO remove that PC.try?
  _ <- PC.manyTill anyToken eof
  return $ catMaybes res

annotateTokens :: ([Token], ann) -> Extractor tag chunk [Annotation TokenizedSentence ann]
annotateTokens tokTags = do
  res <- PC.many $ PC.try $ followedBy anyToken $ annotateToken tokTags  -- TODO remove that PC.try?
  return $ catMaybes res

annotateToken :: ([Token], ann) -> Extractor tag chunk (Maybe (Annotation TokenizedSentence ann))
annotateToken (toks, ann) = do
  input <- fullInput
  anns <- matchTokens toks
  spos <- PC.getPosition
  return $ case anns of
    []         -> Nothing
    (start:xs) -> let endIdx = (PC.sourceColumn spos) - 1 -- PAAAAARRRRSEEEEC!!!!!!!
                      newStart = (endIdx - (length anns))
                  in Just $ Annotation
                       { startIdx = Index newStart
                       , len = length anns
                       , value = ann
                       , payload = input
                       }


-- -- | Consume a token with the given POS Tag
-- posTok :: Tag t => t -> Extractor t (POS t)
-- posTok tag = token showTok posFromTok testTok
--   where
--     showTok      = show
--     posFromTok _ = newPos "unknown" 0 0
--     testTok tok@(POS t _) = if tag == t then Just tok else Nothing

-- -- | A Parsec parser.
-- --
-- -- Example usage:
-- --
-- -- @
-- -- > set -XOverloadedStrings
-- -- > import Text.Parsec.Prim
-- -- > parse myExtractor "interactive repl" someTaggedSentence
-- -- @
-- type Extractor t = Parsec (TaggedSentence t) ()

-- -- | Consume a token with the given POS Tag
-- posTok :: Tag t => t -> Extractor t (POS t)
-- posTok tag = token showTok posFromTok testTok
--   where
--     showTok      = show
--     posFromTok _ = newPos "unknown" 0 0
--     testTok tok@(POS t _) = if tag == t then Just tok else Nothing

-- -- | Consume a token with the specified POS prefix.
-- --
-- -- @
-- -- > parse (posPrefix "n") "ghci" [("Bob", Tag "np")]
-- -- Right [("Bob", Tag "np")]
-- -- @
-- posPrefix :: Tag t => Text -> Extractor t (POS t)
-- posPrefix str = token showTok posFromTok testTok
--   where
--     showTok = show
--     posFromTok _  = newPos "unknown" 0 0
--     testTok tok@(POS t _) = if str `T.isPrefixOf` (tagTerm t) then Just tok else Nothing

-- -- | Text equality matching with optional case sensitivity.
-- matches :: CaseSensitive -> Token -> Token -> Bool
-- matches Sensitive   x y = x == y
-- matches Insensitive (Token x) (Token y) = (T.toLower x) == (T.toLower y)

-- -- | Consume a token with the given lexical representation.
-- txtTok :: Tag t => CaseSensitive -> Token -> Extractor t (POS t)
-- txtTok sensitive txt = token showTok posFromTok testTok
--   where
--     showTok = show
--     posFromTok _  = newPos "unknown" 0 0
--     testTok tok@(POS _ t) | matches sensitive txt t = Just tok
--                           | otherwise               = Nothing

-- oneOf :: Tag t => CaseSensitive -> [Token] -> Extractor t (POS t)
-- oneOf sensitive terms = PC.choice (map (\t -> try (txtTok sensitive t)) terms)

-- | Skips any number of fill tokens, ending with the end parser, and
-- returning the last parsed result.
--
-- This is useful when you know what you're looking for and (for
-- instance) don't care what comes first.
followedBy :: Stream s m t
           => ParsecT s u m a
           -> ParsecT s u m b
           -> ParsecT s u m b
followedBy fill end = do
  _ <- PC.manyTill fill (lookAhead end)
  end


