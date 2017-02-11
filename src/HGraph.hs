{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
-- | A simple example of using an HList to store tokens for parsing.
--
-- > tokenizeNodes $ T.pack "This is a test."
-- Node (Token "This") H[[Node (Token "is") H[[Node (Token "a") H[[Node (Token "test") H[[Node (Token ".") H[[]]]]]]]]]]
-- > showTokens $ tokenizeNodes $ T.pack "This is a test."
-- ["This is a test ."]
--
--
-- Next steps:
--  - Try a tag type (e.g, POS tags)
--  - Write a traversal that looks for, e.g., the token 'a' followed by a Noun.
module HGraph where

import Data.HList
import Data.Text (Text)
import qualified Data.Text as T

import NLP.POS
import NLP.Types
import NLP.Tokenize.Chatter
import qualified NLP.Corpora.Conll as C

-- hard-coding to Conll tags for now to keep the type args down:
type Arcs = HList '[[Node C.Tag], [Node Token]]
-- type Arcs = HList '[[Node Token]]

noEdges :: Arcs
noEdges = [].*.[].*.HNil

-- I really need to find a way to parameterize this on the arcs implementation,
-- but that makes arcs recursively defined, which introduces some problems I haven't
-- figured out yet.
data Node payload = Start Arcs
                  | End
                  | Node payload Arcs
                  deriving (Read, Show)

-- these type signatures are mind-blowingly simple.
-- showTokens :: Node Token -> [Text]
showTokens End = []
showTokens (Start out) =
  case (hOccurs out :: [Node Token]) of
    [] -> [""]
    xs -> concatMap showTokens xs
showTokens (Node (Token str) out) =
  case (hOccurs out :: [Node Token]) of
    [] -> [str]
    xs -> map (\sfx -> T.concat [str, " ", sfx]) $ concatMap showTokens xs


-- tokenizeNodes :: Text -> Node Token
tokenizeNodes txt =
  let (t:ts) = reverse $ tokens $ tokenize txt
      build node tok = Node tok ([].*.[node] .*. HNil)
  in foldl build (Node t noEdges) ts

posTagNodes tgr txt =
  let (Sent toks, tags) = unzipTags $ head $ tag tgr txt
      (x:xs, y:ys) = (reverse toks, reverse tags)

      build (tokNode, tagNode) (tok, tg) =
        ( Node tok ([tagNode] .*. [tokNode] .*. HNil)
        , Node tg ([tagNode] .*. [tokNode] .*. HNil))

      (tokStart, tagStart) = foldl build (Node x noEdges, Node y noEdges) $ zip xs ys

  in Start ([tagStart] .*. [tokStart] .*. HNil)


-- findLetterA (Start out) = concatMap findLetterA out
-- findLetterA End = []
-- findLetterA (Node _ out) =
--   case hOccurs out :: [Node Token] of
--     [] -> concatMap findLetterA out
--     xs -> concatMap getNoun $ filter isTokenA xs

-- getNoun (Start out) = concatMap getNoun out
-- getNoun End = []
-- getNoun (Node tag out) | T.take tag == "N" = [tag] ++ concatMap findLetterA out
--                        | otherwise = concatMap findLetterA out

-- isTokenA :: Node Token -> Bool
-- isTokenA (Node (Token str) rest) = str == T.pack "a"
