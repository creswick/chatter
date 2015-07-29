{-# LANGUAGE OverloadedStrings #-}
module NLP.Tokenize.Annotations
  ( tokenize
  , defaultTokenizer
  , runTokenizer
  , protectTerms
  , whitespace
  , uris
  , punctuation
  , contractions
  , tokenizeOn
  )
where

-- import Control.Monad.Instances ()
import Control.Monad

import Data.Array
import qualified Data.Char as Char
import Data.Function (on)
import Data.List (sortBy, foldl')
import Data.Maybe
import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.TDFA
import Text.Regex.TDFA.Text (compile)

import NLP.Types.Annotations
import NLP.Types (CaseSensitive(..))
import NLP.Tokenize.Types

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

tokenize :: Text -> TokenizedSentence
tokenize txt = runTokenizer defaultTokenizer txt

runTokenizer :: (RawToken -> [RawToken]) -> Text -> TokenizedSentence
runTokenizer tokenizer txt =
  TokenizedSentence { tokText = txt
                    , tokAnnotations = map (toToken txt) (tokenizer (OpenToken 0 txt))
                    }

defaultTokenizer :: RawToken -> [RawToken]
defaultTokenizer = whitespace >=> uris >=> punctuation >=> contractions

-- | Split common contractions off and freeze them.
-- Currently deals with: 'm, 's, 'd, 've, 'll, and negations (n't)
contractions :: RawToken -> [RawToken]
contractions f@(FixedToken _ _) = [f]
contractions t@(OpenToken st txt) =
  case catMaybes . map (splitSuffix txt) $ cts of
    [] -> return t -- no suffix.
    ((w,s):_) -> [ OpenToken st w  -- leave the term open
                 , FixedToken (st + T.length w) s -- fix the suffix
                 ]
  where
    cts = ["'m","'s","'d","'ve","'ll", "n't"]

    -- | Removes the suffix of a string, if the string has the suffix.
    -- Returns `Just (prefix, suffix)` or `Nothing`
    splitSuffix :: Text -> Text -> Maybe (Text, Text)
    splitSuffix w sfx =
      let w' = T.reverse w
          tlen = T.length sfx
      in if sfx `T.isSuffixOf` w
         then Just (T.take (T.length w - tlen) w, T.reverse . T.take tlen $ w')
         else Nothing


punctuation :: RawToken -> [RawToken]
punctuation = leadingPunctuation >=> trailingPunctuation

trailingPunctuation :: RawToken -> [RawToken]
trailingPunctuation f@(FixedToken _ _) = [f]
trailingPunctuation t@(OpenToken st txt) =
  case T.span Char.isPunctuation $ T.reverse txt of
    (ps,w) | T.null ps -> [ t ]
           | T.null  w -> [ FixedToken st $ T.reverse ps ]
           | otherwise -> [ OpenToken st $ T.reverse w
                          , FixedToken (st + T.length w) (T.reverse ps) ]

leadingPunctuation :: RawToken -> [RawToken]
leadingPunctuation f@(FixedToken _ _) = [f]
leadingPunctuation t@(OpenToken st txt) =
  case T.span Char.isPunctuation txt of
    (ps,w) | T.null ps -> [ t ]
           | T.null  w -> [ FixedToken st ps ]
           | otherwise -> [ FixedToken st ps
                          , OpenToken (st + T.length ps) w ]


uris :: RawToken -> [RawToken]
uris f@(FixedToken _ _) = [f]
uris rawTok | isUri (text rawTok) = [FixedToken (start rawTok) (text rawTok)]
            | otherwise           = [rawTok]
  where
    isUri txt = any (`T.isPrefixOf` txt) ["https://", "http://","ftp://","mailto:"]

-- | Tokenize on whitespace, as defined by '\ch -> Char.isSeparator ch || Char.isSpace ch'
whitespace :: RawToken -> [RawToken]
whitespace f@(FixedToken _ _) = [f]
whitespace rawTok = tokenizeOn isSeparator rawTok

-- | Tokenize on characters that satisfy the provided predicate.
tokenizeOn :: (Char -> Bool) -> RawToken -> [RawToken]
tokenizeOn _ f@(FixedToken _ _) = [f]
tokenizeOn isSep rawTok =  reverse $ addLastToken $ T.foldl' fn emptyAcc $ text rawTok
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
      | isSep char && inToken acc =
        acc { curIdx = curIdx acc + 1
            , curStart = 0
            , inToken = False
            , curText = []
            , accTokens = OpenToken { start = curStart acc
                                    , text = T.pack $ reverse $ curText acc }
                          : accTokens acc
            }
      | isSep char && not (inToken acc) =
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

-- | Create a tokenizer that protects the provided terms (to tokenize
-- multi-word terms)
protectTerms :: [Text] -> CaseSensitive -> RawToken -> [RawToken]
protectTerms _ _   f@(FixedToken _ _) = [f]
protectTerms terms sensitive rawToken =
  let sorted = sortBy (compare `on` T.length) $ map escapeRegexChars terms

      sensitivity = case sensitive of
                      Insensitive -> False
                      Sensitive   -> True

      compOption = CompOption
        { caseSensitive = sensitivity
        , multiline = False
        , rightAssoc = True
        , newSyntax = True
        , lastStarGreedy = True
        }

      execOption = ExecOption { captureGroups = False }

      -- The angle brackets here are used to set word boundairies.
      eRegex = compile compOption execOption
                 (T.concat ["\\<", (T.intercalate "\\>|\\<" sorted), "\\>"])

      tokenizeMatches :: Regex -> (RawToken -> [RawToken])
      tokenizeMatches regex tok = case concatMap elems $ matchAllText regex (text tok) of
        [] -> [OpenToken 0 (text tok)]
        xs -> let (anchor, acc) = foldl' foldFn (0, []) xs
                  txtLen = T.length $ text tok
                  lastToken | anchor == txtLen = []
                            | otherwise        = [OpenToken anchor (subStr (text tok) anchor txtLen)]
              in acc <> lastToken

      foldFn ::(Int, [RawToken]) -> (Text, (Int, Int)) -> (Int, [RawToken])
      foldFn (anchor, acc) (tok, (mStart, mLen)) =
        let fixedTokens | mLen == 0 = []
                        | otherwise = [FixedToken mStart tok]
            openTokens | mStart == anchor = []
                       | otherwise = [OpenToken anchor (subStr (text rawToken) anchor mStart)]

        in (mStart + mLen, acc <> openTokens <> fixedTokens)

  in case eRegex of
       Left err -> error ("Regex could not be built: "++err)
       Right rx -> tokenizeMatches rx rawToken

subStr :: Text -> Int -> Int -> Text
subStr txt st end = T.take (end - st) $ T.drop st txt

escapeRegexChars :: Text -> Text
escapeRegexChars input = helper [ "\\", ".", "+", "*", "?", "[", "^", "]", "$"
                                , "(", ")", "{", "}", "=", "!", "<", ">", "|"
                                , ":", "-"
                                ] input

  where
    helper :: [Text] -> Text -> Text
    helper []     term = term
    helper (x:xs) term = helper xs $ escapeChar x term

    escapeChar :: Text -> Text -> Text
    escapeChar char term = T.replace char (T.append "\\" char) term
