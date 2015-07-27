{-# LANGUAGE OverloadedStrings #-}
module NLP.Tokenize.TextTrie
  where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ListTrie.Patricia.Set as PS

import NLP.Tokenize.Types (RawToken(..))

testTrie :: PS.TrieSet M.Map Char
testTrie = let t1 = PS.singleton "Test"
               t2 = PS.insert "Bar" t1
               t3 = PS.insert "Testing" t2
           in t3

buildTrie :: [Text] -> PS.TrieSet M.Map Char
buildTrie toks = PS.fromList $ map T.unpack toks


tokens :: [Text]
tokens = [ "Galois, Inc."
         , "Microsoft, Inc."
         , "Apache Tomcat"
         , "n't"
         , "Mr."
         , "Dr."
         , "\""
         , "testing"
         , "test"
         ]

theTrie = buildTrie tokens

trieChildren1 :: PS.TrieSet M.Map Char -> M.Map Char (PS.TrieSet M.Map Char)
trieChildren1 trie = let m = PS.children1 trie
                     in if M.size m > 0
                        then m
                        else mkMap trie
  where
    -- Only called when tr has one or zero entries.
    mkMap :: PS.TrieSet M.Map Char -> M.Map Char (PS.TrieSet M.Map Char)
    mkMap tr = case take 1 $ PS.toList tr of
                 ((x:xs):_) -> M.fromList [(x, PS.singleton xs)]
                 _ -> M.empty

-- | Build a tokenizer that tokenizes the supplied tokens, preffering longer matches where possible.
protectedTokenizer :: [Text] -> RawToken -> [RawToken]
protectedTokenizer terms = protectedTokens $ buildTrie terms

-- | The incomming trie contains all strings that need to be tokenized
-- specially.
--
-- TODO: Fails on the trie above [(test, testing)] and an input like "this is a testin thing"
protectedTokens :: PS.TrieSet M.Map Char -> RawToken -> [RawToken]
protectedTokens trie rawTok = reverse $ addLastToken $ T.foldl' fn emptyAcc $ text rawTok

  where
    emptyAcc = Acc { curIdx = start rawTok
                   , curStart = 0
                   , inToken = False
                   , tokBuilder = OpenToken
                   , curText = []
                   , accTokens = []
                   , curTrie = trie
                   }

    addLastToken :: Accumulator -> [RawToken]
    addLastToken acc = ((tokBuilder acc) (curStart acc) (T.pack $ reverse $ curText acc))
                       : accTokens acc

    -- | Accumulator to track and change states.
    --
    -- States:
    --  * Starting state.
    --  * In a special token
    --    * Cur char matches one of the the next Trie chars
    --      * Cur char finishes a special token.
    --      * Cur char is in the middle of a special token.
    --    * Cur char does *not* match one of the next trie chars
    --    * Trie is empty.
    --  * Not in a special token.
    --    * Cur char matches one of the the next Trie char
    --    * Cur char does *not* match one of the next trie chars
    --  * No more tokens.
    fn :: Accumulator -> Char -> Accumulator
    fn acc char = case M.lookup char $ trieChildren1 (curTrie acc) of
      Nothing | inToken acc ->
                 case PS.member (reverse $ curText acc) trie of
                   True -> acc { curIdx = curIdx acc + 1
                               , curStart = curIdx acc
                               , inToken = False
                               , tokBuilder = OpenToken
                               , curText = [char]
                               , accTokens = case curText acc of
                                               [] -> accTokens acc
                                               _ -> FixedToken { start = curStart acc
                                                               , text = T.pack $ reverse $ curText acc
                                                               }
                                                    : accTokens acc
                               , curTrie = trie -- reset the trie
                               }
                   -- Nope! we are in an open token afterall (just finished a *prefix* of something in the trie)
                   False -> acc { curIdx = curIdx acc + 1
                                , inToken = False
                                , tokBuilder = OpenToken
                                , curText = char:curText acc
                                , curTrie = trie -- reset the trie
                                }
              | otherwise   -> -- Still not in a special token:
                 acc { curIdx = curIdx acc + 1
                     , curText = char:curText acc -- append char to the current token
                     }

      Just  t | inToken acc -> -- in a token:
                 acc { curIdx = curIdx acc + 1 -- update index.
                     , curText = char:curText acc -- append char to the current token
                     , curTrie = t                -- update the trie state.
                     }
              | otherwise -> -- *starting* a special token.
                 acc { curIdx = curIdx acc + 1
                     , curStart = curIdx acc
                     , inToken = True
                     , tokBuilder = FixedToken
                     , curText = [char]
                     , accTokens = case curText acc of
                                     [] -> accTokens acc
                                     _ -> OpenToken { start = curStart acc
                                                    , text = T.pack $ reverse $ curText acc }
                                                    : accTokens acc
                     , curTrie = t
                     }

data Accumulator = Acc { curIdx :: !Int
                       , curStart :: !Int
                       , inToken :: !Bool
                       , tokBuilder :: Int -> Text -> RawToken
                       , curText :: [Char]
                       , accTokens :: [RawToken]
                       , curTrie :: PS.TrieSet M.Map Char
--                       , longestToken :: [Char]
                       }









