{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

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

import NLP.Types
import NLP.Tokenize.Chatter

-- with POS tags, this becomes:
-- type Args tag = HList '[[Node tag], [Node Token]]
type Arcs = HList '[[Node Token]]

-- I really need to find a way to parameterize this on the arcs implementation,
-- but that makes arcs recursively defined, which introduces some problems I haven't
-- figured out yet.
data Node payload = Start Arcs
                  | Node payload Arcs
                  deriving (Read, Show)

-- these type signatures are mind-blowingly simple.
showTokens :: Node Token -> [Text]
showTokens (Start out) =
  case (hOccurs out :: [Node Token]) of
    [] -> [""]
    xs -> concatMap showTokens xs
showTokens (Node (Token str) out) =
  case (hOccurs out :: [Node Token]) of
    [] -> [str]
    xs -> map (\sfx -> T.concat [str, " ", sfx]) $ concatMap showTokens xs

noEdges :: Arcs
noEdges = [].*.HNil

tokenizeNodes :: Text -> Node Token
tokenizeNodes txt =
  let (t:ts) = reverse $ tokens $ tokenize txt
      build node tok = Node tok ([node] .*. HNil)
  in foldl build (Node t noEdges) ts
