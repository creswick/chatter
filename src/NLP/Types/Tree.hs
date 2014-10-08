{-# LANGUAGE OverloadedStrings #-}
module NLP.Types.Tree where

import Prelude hiding (print)
import Data.List (intercalate)

import qualified NLP.Corpora.Brown as B
import NLP.Types hiding (Sentence, TaggedSentence)

data ChunkedSentence chunk tag = ChunkedSent [ChunkOr chunk tag]
  deriving (Read, Show, Eq)

data TaggedSentence tag = TaggedSent [TagOr tag]
  deriving (Read, Show, Eq)

data Sentence = Sent [Token]
  deriving (Read, Show, Eq)

data ChunkOr chunk tag = Chunk_CN (Chunk chunk tag)
                       | POS_CN   (POS tag)
                       | Token_CN Token
                         deriving (Read, Show, Eq)

data TagOr tag = POS_TN   (POS tag)
               | Token_TN Token
                 deriving (Read, Show, Eq)

data Chunk chunk tag = Chunk chunk [ChunkOr chunk tag]
  deriving (Read, Show, Eq)

data POS tag = POS tag Token
  deriving (Read, Show, Eq)

data Token = Token String
  deriving (Read, Show, Eq)

-- (S (NP (NN I)) (VP (V saw) (NP (NN him))))
t1 :: Sentence
t1 = Sent
     [ Token "I"
     , Token "saw"
     , Token "him"
     , Token "."
     ]

t2 :: TaggedSentence B.Tag
t2 = TaggedSent
     [ POS_TN (POS B.NN    (Token "I"))
     , POS_TN (POS B.VB    (Token "saw"))
     , POS_TN (POS B.NN    (Token "him"))
     , POS_TN (POS B.Term  (Token "."))
     ]

t3 :: ChunkedSentence B.Chunk B.Tag
t3 = ChunkedSent
     [ Chunk_CN (Chunk B.C_NP [ POS_CN (POS B.NN (Token "I"))])
     , Chunk_CN (Chunk B.C_VP [ POS_CN (POS B.VB   (Token "saw"))
                            , POS_CN (POS B.NN  (Token "him"))
                            ]
                 )
     , POS_CN (POS B.Term (Token "."))
     ]

