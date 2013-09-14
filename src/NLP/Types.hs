module NLP.Types where

import Data.Text (Text)

-- | Part of Speech tags as defined by the Penn Treebank corpora, but
-- with the addition of an "Other" tag that takes an unconstrained
-- Text value.
data POSTag = CC -- ^ Coordinating conjunction
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
            | PRPS -- ^ (PRP$) Possessive pronoun
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
            | WPS -- ^ Possessive wh-pronoun (WP$)
            | WRB -- ^ Wh-adverb
            | Other Text
              deriving (Read, Show, Eq)