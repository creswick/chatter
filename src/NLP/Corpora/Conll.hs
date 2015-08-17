{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Data types representing the POS tags and Chunk tags derived from
-- the Conll2000 training corpus.
module NLP.Corpora.Conll where

import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readEither)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

import GHC.Generics

import qualified NLP.Types.Annotations as A
import NLP.Types.General

-- | Named entity categories defined for the Conll 2003 task.
data NERTag = PER
            | ORG
            | LOC
            | MISC
  deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

instance Arbitrary NERTag where
  arbitrary = elements [minBound..]

instance Serialize NERTag
instance A.NamedEntity NERTag
instance Hashable NERTag

-- | Phrase chunk tags defined for the Conll task.
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
           | O -- ^ "out"; not a chunk.
  deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

instance Hashable Chunk

instance Arbitrary Chunk where
  arbitrary = elements [minBound..]

instance Serialize Chunk
instance A.HasMarkup Chunk where
  getMarkup = A.chunkMarkup

instance A.POS Tag where
  serializePOS = showTag

  parsePOS txt = readTag txt

  -- | Constant tag for "unknown"
  tagUNK = Unk


  startPOS = START
  endPOS = END

  isDt tag = tag `elem` [DT]

instance A.HasMarkup Tag where
  getMarkup = A.posMarkup

instance Arbitrary Tag where
  arbitrary = elements [minBound ..]
instance Serialize Tag
instance Hashable Tag

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
  in toEitherErr (readEither $ T.unpack normalized)

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

instance A.Chunk Chunk where
  serializeChunk = T.pack . show
  parseChunk txt = toEitherErr $ readEither $ T.unpack txt
  notChunk = O

-- | These tags may actually be the Penn Treebank tags.  But I have
-- not (yet?) seen the punctuation tags added to the Penn set.
--
-- This particular list was complied from the union of:
--
--   * All tags used on the Conll2000 training corpus. (contributing the punctuation tags)
--   * The PennTreebank tags, listed here: <https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html> (which contributed LS over the items in the corpus).
--   * The tags: START, END, and Unk, which are used by Chatter.
--
data Tag = START -- ^ START tag, used in training.
         | END -- ^ END tag, used in training.
         | Hash -- ^ #
         | Dollar -- ^ $
         | CloseDQuote -- ^ ''
         | OpenDQuote -- ^ ``
         | Op_Paren -- ^ (
         | Cl_Paren -- ^ )
         | Comma -- ^ ,
         | Term -- ^ . Sentence Terminator
         | Colon -- ^ :
         | CC -- ^ Coordinating conjunction
         | CD -- ^ Cardinal number
         | DT -- ^ Determiner
         | EX -- ^ Existential there
         | FW -- ^ Foreign word
         | IN -- ^ Preposition or subordinating conjunction
         | JJ -- ^ Adjective
         | JJR -- ^ Adjective, comparative
         | JJS -- ^ Adjective, superlative
         | LS -- ^ List item marker
         | MD -- ^ Modal
         | NN -- ^ Noun, singular or mass
         | NNS -- ^ Noun, plural
         | NNP -- ^ Proper noun, singular
         | NNPS -- ^ Proper noun, plural
         | PDT -- ^ Predeterminer
         | POS -- ^ Possessive ending
         | PRP -- ^ Personal pronoun
         | PRPdollar -- ^ Possessive pronoun
         | RB -- ^ Adverb
         | RBR -- ^ Adverb, comparative
         | RBS -- ^ Adverb, superlative
         | RP -- ^ Particle
         | SYM -- ^ Symbol
         | TO -- ^ to
         | UH -- ^ Interjection
         | VB -- ^ Verb, base form
         | VBD -- ^ Verb, past tense
         | VBG -- ^ Verb, gerund or present participle
         | VBN -- ^ Verb, past participle
         | VBP -- ^ Verb, non-3rd person singular present
         | VBZ -- ^ Verb, 3rd person singular present
         | WDT -- ^ Wh-determiner
         | WP -- ^ Wh-pronoun
         | WPdollar -- ^ Possessive wh-pronoun
         | WRB -- ^ Wh-adverb
         | Unk
  deriving (Read, Show, Ord, Eq, Generic, Enum, Bounded)

