{-# LANGUAGE OverloadedStrings #-}
module NLP.POS.LiteralTagger
    ( tag
    , tagSentence
    , mkTagger
    , taggerID
    , readTagger
    , CaseSensitive(..)
    , protectTerms
    )
where

import Control.Monad ((>=>))
import Data.Array
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Serialize (encode, decode)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T

import NLP.Tokenize.Text (Tokenizer, EitherList(..), defaultTokenizer, run)
import NLP.FullStop (segment)
import NLP.Types ( tagUNK, Sentence, TaggedSentence(..)
                 , Tag, POSTagger(..), CaseSensitive(..))
import Text.Regex.TDFA
import Text.Regex.TDFA.Text (compile)

taggerID :: ByteString
taggerID = pack "NLP.POS.LiteralTagger"


-- | Create a Literal Tagger using the specified back-off tagger as a
-- fall-back, if one is specified.
--
-- This uses a tokenizer adapted from the 'tokenize' package for a
-- tokenizer, and Erik Kow's fullstop sentence segmenter as a sentence
-- splitter.
mkTagger :: Tag t => Map Text t -> CaseSensitive -> Maybe (POSTagger t) -> POSTagger t
mkTagger table sensitive mTgr = POSTagger
  { posTagger  = tag (canonicalize table) sensitive
  , posTrainer = \_ -> return $ mkTagger table sensitive mTgr
  , posBackoff = mTgr
  , posTokenizer = run (protectTerms (Map.keys table) sensitive >=> defaultTokenizer)
  , posSplitter = (map T.pack) . segment . T.unpack
  , posSerialize = encode (table, sensitive)
  , posID = taggerID
  }
  where canonicalize :: Tag t => Map Text t -> Map Text t
        canonicalize =
          case sensitive of
            Sensitive   -> id
            Insensitive -> Map.mapKeys T.toLower

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

-- | Create a tokenizer that protects the provided terms (to tokenize
-- multi-word terms)
protectTerms :: [Text] -> CaseSensitive -> Tokenizer
protectTerms terms sensitive =
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

      eRegex = compile compOption execOption
                 (T.concat ["\\<", (T.intercalate "\\>|\\<" sorted), "\\>"])

      toEithers :: [(Int, Int)] -> Text -> [Either Text Text]
      toEithers []                str = [Right str]
      toEithers ((idx, len):rest) str =
        let (first, theTail)      = T.splitAt idx str
            (token, realTail) = T.splitAt len theTail
            scaledRest        = map (\(x,y)->((x-(idx+len)), y)) rest
        in filterEmpty ([Right first, Left token] ++ (toEithers scaledRest realTail))

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

tag :: Tag t => Map Text t -> CaseSensitive -> [Sentence] -> [TaggedSentence t]
tag table sensitive ss = map (tagSentence table sensitive) ss

tagSentence :: Tag t => Map Text t -> CaseSensitive -> Sentence -> TaggedSentence t
tagSentence table sensitive toks = TS (map findTag toks)
  where
--    findTag :: Tag t => Text -> (Text, t)
    findTag txt = (txt, Map.findWithDefault tagUNK (canonicalize txt) table)

    canonicalize :: Text -> Text
    canonicalize =
      case sensitive of
        Sensitive   -> id
        Insensitive -> T.toLower

-- | deserialization for Literal Taggers.  The serialization logic is
-- in the posSerialize record of the POSTagger created in mkTagger.
readTagger :: Tag t => ByteString -> Maybe (POSTagger t) -> Either String (POSTagger t)
readTagger bs backoff = do
  (model, sensitive) <- decode bs
  return $ mkTagger model sensitive backoff
