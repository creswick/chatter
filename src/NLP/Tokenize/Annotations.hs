{-# LANGUAGE OverloadedStrings #-}
module NLP.Tokenize.Annotations
  ( tokenize
  , defaultTokenizer
  , runTokenizer
  , protectTerms
  )
where

import Data.Array
import qualified Data.Char as Char
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe
import Control.Monad.Instances ()
import Control.Applicative
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.TDFA
import Text.Regex.TDFA.Text (compile)

import NLP.Types.Annotations
import NLP.Types (CaseSensitive(..))
import NLP.Tokenize.Types
import NLP.Tokenize.TextTrie (protectTerms)

toToken :: Text -> RawToken -> Annotation Text Token
toToken doc tok = Annotation { startIdx = Index $ start tok
                             , len = T.length (text tok)
                             , value = Token (text tok)
                             , payload = doc
                             }

data Accumulator = Acc { curIdx :: !Int
                       , curStart :: !Int
                       , inToken :: !Bool
                       , curText :: [Char]
                       , accTokens :: [RawToken]
                       }

tokenize :: Tokenizer
tokenize txt = runTokenizer defaultTokenizer txt

runTokenizer :: (RawToken -> [RawToken]) -> Tokenizer
runTokenizer tokenizer txt =
  TokSentence { tokText = txt
              , tokAnnotations = map (toToken txt) (tokenizer (OpenToken 0 txt))
              }

defaultTokenizer :: RawToken -> [RawToken]
defaultTokenizer = whitespace >=> uris >=> punctuation >=> contractions

-- | Split common contractions off and freeze them.
-- Currently deals with: 'm, 's, 'd, 've, 'll, and negations (n't)
contractions :: RawToken -> [RawToken]
contractions f@(FixedToken _ _) = [f]
contractions t@(OpenToken start txt) =
  case catMaybes . map (splitSuffix txt) $ cts of
    [] -> return t -- no suffix.
    ((w,s):_) -> [ OpenToken start w  -- leave the term open
                 , FixedToken (start + T.length w) s -- fix the suffix
                 ]
  where
    cts = ["'m","'s","'d","'ve","'ll", "n't"]

    -- | Removes the suffix of a string, if the string has the suffix.
    -- Returns `Just (prefix, suffix)` or `Nothing`
    splitSuffix :: Text -> Text -> Maybe (Text, Text)
    splitSuffix w sfx =
      let w' = T.reverse w
          len = T.length sfx
      in if sfx `T.isSuffixOf` w
         then Just (T.take (T.length w - len) w, T.reverse . T.take len $ w')
         else Nothing


punctuation :: RawToken -> [RawToken]
punctuation = leadingPunctuation >=> trailingPunctuation

trailingPunctuation :: RawToken -> [RawToken]
trailingPunctuation f@(FixedToken _ _) = [f]
trailingPunctuation t@(OpenToken start txt) =
  case T.span Char.isPunctuation $ T.reverse txt of
    (ps,w) | T.null ps -> [ t ]
           | T.null  w -> [ FixedToken start $ T.reverse ps ]
           | otherwise -> [ OpenToken start $ T.reverse w
                          , FixedToken (start + T.length w) (T.reverse ps) ]

leadingPunctuation :: RawToken -> [RawToken]
leadingPunctuation f@(FixedToken _ _) = [f]
leadingPunctuation t@(OpenToken start txt) =
  case T.span Char.isPunctuation txt of
    (ps,w) | T.null ps -> [ t ]
           | T.null  w -> [ FixedToken start ps ]
           | otherwise -> [ FixedToken start ps
                          , OpenToken (start + T.length ps) w ]


uris :: RawToken -> [RawToken]
uris f@(FixedToken _ _) = [f]
uris rawTok | isUri (text rawTok) = [FixedToken (start rawTok) (text rawTok)]
            | otherwise           = [rawTok]
  where
    isUri txt = any (`T.isPrefixOf` txt) ["https://", "http://","ftp://","mailto:"]

whitespace :: RawToken -> [RawToken]
whitespace f@(FixedToken _ _) = [f]
whitespace rawTok = reverse $ addLastToken $ T.foldl' fn emptyAcc $ text rawTok
  where
    emptyAcc = Acc { curIdx = start rawTok
                   , curStart = 0
                   , inToken = False
                   , curText = []
                   , accTokens = []
                   }

    addLastToken :: Accumulator -> [RawToken]
    addLastToken acc | not (inToken acc) = accTokens acc
                     | otherwise = OpenToken { start = curStart acc
                                             , text = T.pack $ reverse $ curText acc }
                                   :(accTokens acc)


    fn :: Accumulator -> Char -> Accumulator
    fn acc char
      | isSeparator char && inToken acc =
        acc { curIdx = curIdx acc + 1
            , curStart = 0
            , inToken = False
            , curText = []
            , accTokens = OpenToken { start = curStart acc
                                    , text = T.pack $ reverse $ curText acc }
                          : accTokens acc
            }
      | isSeparator char && not (inToken acc) =
        acc { curIdx = curIdx acc + 1 }
      | not (isSeparator char) && not (inToken acc) =
        acc { curIdx = curIdx acc + 1
            , curStart = curIdx acc
            , inToken = True
            , curText = char:curText acc
            }
      | otherwise =
        acc { curIdx = curIdx acc + 1
            , inToken = True
            , curText = char:curText acc
            }

isSeparator :: Char -> Bool
isSeparator ch = (Char.isSeparator ch || Char.isSpace ch)

