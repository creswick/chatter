{-# LANGUAGE OverloadedStrings #-}
-- | This module aims to make tagging text with parts of speech
-- trivially easy.  If you're new to 'chatter' and POS-tagging, then I
-- suggest you simply try:
--
-- >>> tagStr defaultTagger "This is a sample sentence"
--
-- Note that you used 'tagStr', instead of 'tag'.  Many people don't
-- (yet!) use "Data.Text" by default, so there is a wrapper around
-- 'tag' that packs and unpacks the 'String'.  This is innefficient,
-- but it's just to get you started, and 'tagStr' can be very handy
-- when you're debugging an tagger in ghci (or cabal repl).
--
module NLP.POS
  ( tag
  , tagStr
  , tagText
  )
where

import NLP.Types (TaggedSentence, Tag(..)
                 , POSTagger(..), tagUNK)

import Data.Text (Text)
import qualified Data.Text as T

-- | Tag a chunk of input text with part-of-speech tags, using the
-- sentence splitter, tokenizer, and tagger contained in the 'POSTager'.
tag :: POSTagger -> Text -> [TaggedSentence]
tag posTagger txt = let sentences = (sentSplitter posTagger) txt
                        tokens    = map (tokenizer posTagger) sentences
                        priority  = (tagger posTagger) tokens
                    in case backoff posTagger of
                         Nothing  -> priority
                         Just tgr -> combine priority (tag tgr txt)

combine :: [TaggedSentence] -> [TaggedSentence] -> [TaggedSentence]
combine xs ys = zipWith combineSentences xs ys

combineSentences :: TaggedSentence -> TaggedSentence -> TaggedSentence
combineSentences xs ys = zipWith pickTag xs ys

pickTag :: (Text, Tag) -> (Text, Tag) -> (Text, Tag)
pickTag (txt1, t1) (txt2, t2) | txt1 /= txt2 = error "Text does not match"
                              | t1 /= tagUNK = (txt1, t1)
                              | otherwise    = (txt1, t2)

-- | Tag the tokens in a string.
--
-- Returns a space-separated string of tokens, each token suffixed
-- with the part of speech.  For example:
--
-- >>> tag tagger "the dog jumped ."
-- "the/at dog/nn jumped/vbd ./."
--
tagStr :: POSTagger -> String -> String
tagStr tgr = T.unpack . tagText tgr . T.pack

-- | Text version of tagStr
tagText :: POSTagger -> Text -> Text
tagText tgr str = T.intercalate " " $ map toTaggedTok taggedSents
  where
    taggedSents = concat $ tag tgr str

    toTaggedTok :: (Text, Tag) -> Text
    toTaggedTok (tok, Tag c) = tok `T.append` (T.cons '/' c)