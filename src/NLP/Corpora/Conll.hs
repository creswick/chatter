{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Data types representing the POS tags and Chunk tags derived from
-- the Conll2000 training corpus.
module NLP.Corpora.Conll where

import Data.Serialize (Serialize)
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readMaybe)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import GHC.Generics

import qualified NLP.Types.Tags as T
import NLP.Types.General

data Chunk = ADJP
           | ADVP
           | CONJP
           | INTJ
           | LST
           | NP -- ^ Noun Phrase.
           | PP -- ^ Prepositional Phrase.
           | PRT
           | SBAR
           | UCP
           | VP -- ^ Verb Phrase.
  deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

instance Arbitrary Chunk where
  arbitrary = elements [minBound..]

instance Serialize Chunk

instance Serialize Tag

instance T.Tag Tag where
  fromTag = showTag

  parseTag txt = case readTag txt of
                   Left  _ -> Unk
                   Right t -> t

  -- | Constant tag for "unknown"
  tagUNK = Unk

  tagTerm = showTag

instance Arbitrary Tag where
  arbitrary = elements [minBound ..]

readTag :: Text -> Either Error Tag
readTag "#" = Right Hash
readTag "$" = Right Dollar
readTag "(" = Right Op_Paren
readTag ")" = Right Cl_Paren
readTag "''" = Right CloseDQuote
readTag "``" = Right OpenDQuote
readTag "," = Right Comma
readTag "." = Right Term
readTag ":" = Right Colon
readTag txt =
  let normalized = replaceAll tagTxtPatterns (T.toUpper txt)
  in case readMaybe $ T.unpack normalized of
       Nothing -> Left (T.append "Could not parse: " txt)
       Just  t -> Right t

-- | Order matters here: The patterns are replaced in reverse order
-- when generating tags, and in top-to-bottom when generating tags.
tagTxtPatterns :: [(Text, Text)]
tagTxtPatterns = [ ("$", "dollar")
                 ]

reversePatterns :: [(Text, Text)]
reversePatterns = map (\(x,y) -> (y,x)) tagTxtPatterns

showTag :: Tag -> Text
showTag Hash = "#"
showTag Op_Paren = "("
showTag Cl_Paren = ")"
showTag CloseDQuote = "''"
showTag OpenDQuote = "``"
showTag Dollar = "$"
showTag Comma = ","
showTag Term = "."
showTag Colon = ":"
showTag tag = replaceAll reversePatterns (T.pack $ show tag)

replaceAll :: [(Text, Text)] -> (Text -> Text)
replaceAll patterns = foldl (.) id (map (uncurry T.replace) patterns)

instance T.ChunkTag Chunk where
  fromChunk = T.pack . show

data Tag = Hash -- ^ #
         | Dollar -- ^ $
         | CloseDQuote -- ^ ''
         | OpenDQuote -- ^ ``
         | Op_Paren -- ^ (
         | Cl_Paren -- ^ )
         | Comma -- ^ ,
         | Term -- ^ . Sentence Terminator
         | Colon -- ^ :
         | CC
         | CD
         | DT
         | EX
         | FW
         | IN
         | JJ
         | JJR
         | JJS
         | MD
         | NN
         | NNP
         | NNPS
         | NNS
         | PDT
         | POS
         | PRP
         | PRPdollar
         | RB
         | RBR
         | RBS
         | RP
         | SYM
         | TO
         | UH
         | VB
         | VBD
         | VBG
         | VBN
         | VBP
         | VBZ
         | WDT
         | WP
         | WPdollar
         | WRB
         | Unk
  deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)
