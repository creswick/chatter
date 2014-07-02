{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module NLP.POS.LiteralTagger
#ifndef __TEST__
    -- ( tag
    -- , tagSentence
    -- , mkTagger
    -- , taggerID
    -- , readTagger
    -- , CaseSensitive(..)
    -- )
#endif
where


import GHC.Generics

import Control.Monad ((>=>))
import Data.Array
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize, encode, decode)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T

import NLP.Tokenize.Text (tokenize, Tokenizer, EitherList(..), defaultTokenizer, run)
import NLP.FullStop (segment)
import NLP.Types ( tagUNK, Sentence, TaggedSentence
                 , Tag, POSTagger(..))
import Text.Regex.TDFA
import Text.Regex.TDFA.Text (compile)

taggerID :: ByteString
taggerID = pack "NLP.POS.LiteralTagger"

data CaseSensitive = Sensitive | Insensitive
  deriving (Read, Show, Generic)

instance Serialize CaseSensitive

-- | Create a Literal Tagger using the specified back-off tagger as a
-- fall-back, if one is specified.
--
-- This uses a tokenizer adapted from the 'tokenize' package for a
-- tokenizer, and Erik Kow's fullstop sentence segmenter as a sentence
-- splitter.
mkTagger :: Map Text Tag -> CaseSensitive -> Maybe POSTagger -> POSTagger
mkTagger table sensitive mTgr = POSTagger
  { posTagger  = tag (canonicalize table) sensitive
  , posTrainer = \_ -> return $ mkTagger table sensitive mTgr
  , posBackoff = mTgr
  , posTokenizer = run (protectTerms (Map.keys table) >=> defaultTokenizer)
  , posSplitter = (map T.pack) . segment . T.unpack
  , posSerialize = encode (table, sensitive)
  , posID = taggerID
  }
  where canonicalize :: Map Text Tag -> Map Text Tag
        canonicalize =
          case sensitive of
            Sensitive   -> id
            Insensitive -> Map.mapKeys T.toLower


-- | Create a tokenizer that protects the provided terms (to tokenize
-- multi-word terms)
protectTerms :: [Text] -> Tokenizer
protectTerms terms =
  let sorted = sortBy (compare `on` T.length) terms

      compOption = CompOption
        { caseSensitive = True
        , multiline = False
        , rightAssoc = True
        , newSyntax = True
        , lastStarGreedy = True
        }

      execOption = ExecOption { captureGroups = False }

      eRegex = compile compOption execOption (T.concat ["\\<", (T.intercalate "\\>|\\<" sorted), "\\>"])

      toEithers :: [(Int, Int)] -> Text -> [Either Text Text]
      toEithers []                str = [Right str]
      toEithers ((idx, len):rest) str =
        let (head, tail)      = T.splitAt idx str
            (token, realTail) = T.splitAt len tail
            scaledRest        = map (\(x,y)->((x-(idx+len)), y)) rest
        in filterEmpty ([Right head, Left token] ++ (toEithers scaledRest realTail))

      filterEmpty :: [Either Text Text] -> [Either Text Text]
      filterEmpty []            = []
      filterEmpty (Left "":xs)  = filterEmpty xs
      filterEmpty (Right "":xs) = filterEmpty xs
      filterEmpty (x:xs)        = x:filterEmpty xs

      tokenizeMatches :: Regex -> Tokenizer
      tokenizeMatches regex tok =
        E (toEithers (concatMap elems $ matchAll regex tok) tok)
  in case eRegex of
       Left err -> error ("Regex could not be built: "++err)
       Right rx -> tokenizeMatches rx

 -- x | isUri x = E [Left x]
 --       | True    = E [Right x]
 --    where isUri u = any (`T.isPrefixOf` u) ["http://","ftp://","mailto:"]

tag :: Map Text Tag -> CaseSensitive -> [Sentence] -> [TaggedSentence]
tag table sensitive ss = map (tagSentence table sensitive) ss

tagSentence :: Map Text Tag -> CaseSensitive -> Sentence -> TaggedSentence
tagSentence table sensitive toks = map findTag toks
  where
    findTag :: Text -> (Text, Tag)
    findTag txt = (txt, Map.findWithDefault tagUNK (canonicalize txt) table)

    canonicalize :: Text -> Text
    canonicalize =
      case sensitive of
        Sensitive   -> id
        Insensitive -> T.toLower

-- | deserialization for Literal Taggers.  The serialization logic is
-- in the posSerialize record of the POSTagger created in mkTagger.
readTagger :: ByteString -> Maybe POSTagger -> Either String POSTagger
readTagger bs backoff = do
  (model, sensitive) <- decode bs
  return $ mkTagger model sensitive backoff