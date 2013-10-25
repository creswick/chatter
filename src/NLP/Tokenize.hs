{-# LANGUAGE OverloadedStrings #-}
-- | NLP Tokenizer, adapted to use Text instead of Strings from the
-- `tokenize` package:
--  * http://hackage.haskell.org/package/tokenize-0.1.3
module NLP.Tokenize
    ( EitherList(..)
    , Tokenizer
    , tokenize
    , run
    , defaultTokenizer
    , whitespace
    , uris
    , punctuation
    , finalPunctuation
    , initialPunctuation
    , contractions
    , negatives
    )
where

import qualified Data.Char as Char
import Data.List
import Data.Maybe
import Control.Monad.Instances
import Data.List.Split
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T

-- | A Tokenizer is function which takes a list and returns a list of Eithers
--  (wrapped in a newtype). Right Strings will be passed on for processing
--  to tokenizers down
--  the pipeline. Left Strings will be passed through the pipeline unchanged.
--  Use a Left String in a tokenizer to protect certain tokens from further 
--  processing (e.g. see the 'uris' tokenizer).
type Tokenizer =  Text -> EitherList Text Text

-- | The EitherList is a newtype-wrapped list of Eithers.
newtype EitherList a b =  E { unE :: [Either a b] }

-- | Split string into words using the default tokenizer pipeline 
tokenize :: Text -> [Text]
tokenize  = run defaultTokenizer

-- | Run a tokenizer
run :: Tokenizer -> (Text -> [Text])
run f = map unwrap . unE . f

defaultTokenizer :: Tokenizer
defaultTokenizer =     whitespace 
                   >=> uris 
                   >=> hyphens
                   >=> punctuation 
                   >=> contractions 
                   >=> negatives 

-- | Detect common uris and freeze them
uris :: Tokenizer
uris x | isUri x = E [Left x]
       | True    = E [Right x]
    where isUri x = any (`T.isPrefixOf` x) ["http://","ftp://","mailto:"]

-- | Split off initial and final punctuation
punctuation :: Tokenizer 
punctuation = finalPunctuation >=> initialPunctuation

hyphens :: Tokenizer
hyphens xs = E [Right w | w <- T.split (=='-') xs ]

-- | Split off word-final punctuation
finalPunctuation :: Tokenizer
finalPunctuation x = E $ filter (not . T.null . unwrap) res
  where
    res :: [Either Text Text]
    res = case T.span Char.isPunctuation (T.reverse x) of
      (ps, w) | T.null ps -> [ Right $ T.reverse w ]
              | otherwise -> [ Right $ T.reverse w
                             , Right $ T.reverse ps]
      -- ([],w) -> [Right . T.reverse $ w]
      -- (ps,w) -> [Right . T.reverse $ w, Right . T.reverse $ ps]

-- | Split off word-initial punctuation
initialPunctuation :: Tokenizer
initialPunctuation x = E $ filter (not . T.null . unwrap) $
    case T.span Char.isPunctuation x of
      (ps,w) | T.null ps -> [ Right w ]
             | otherwise -> [ Right ps
                            , Right w ]

-- | Split words ending in n't, and freeze n't 
negatives :: Tokenizer
negatives x | "n't" `T.isSuffixOf` x = E [ Right . T.reverse . T.drop 3 . T.reverse $ x
                                         , Left "n't" ]
            | True                   = E [ Right x ]

-- | Split common contractions off and freeze them.
-- | Currently deals with: 'm, 's, 'd, 've, 'll
contractions :: Tokenizer
contractions x = case catMaybes . map (splitSuffix x) $ cts of
                   [] -> return x
                   ((w,s):_) -> E [ Right w,Left s]
    where cts = ["'m","'s","'d","'ve","'ll"]
          splitSuffix w sfx = 
              let w' = T.reverse w
                  len = T.length sfx
              in if sfx `T.isSuffixOf` w 
                 then Just (T.take (T.length w - len) w, T.reverse . T.take len $ w')
                 else Nothing


-- | Split string on whitespace. This is just a wrapper for Data.List.words
whitespace :: Tokenizer
whitespace xs = E [Right w | w <- T.words xs ]

instance Monad (EitherList a) where
    return x = E [Right x]
    E xs >>= f = E $ concatMap (either (return . Left) (unE . f)) xs

unwrap :: Either a a -> a
unwrap (Left x) = x
unwrap (Right x) = x

examples = 
    ["This shouldn't happen."
    ,"Some 'quoted' stuff"
    ,"This is a URL: http://example.org."
    ,"How about an email@example.com"
    ,"ReferenceError #1065 broke my debugger!"
    ,"I would've gone."
    ,"They've been there."
    ]

