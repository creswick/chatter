{-# LANGUAGE OverloadedStrings #-}
module NLP.POS where


import NLP.Types (TaggedSentence, Tag(..), POSTagger(..)
                 , tagUNK, Sentence)

import Data.Text (Text)
import qualified Data.Text as T

tag :: POSTagger -> Text -> [TaggedSentence]
tag posTagger txt = let tokens   = tokenize txt
                        priority = (tagger posTagger) tokens
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

tokenize :: Text -> [Sentence]
tokenize txt = map T.words $ T.lines txt

-- | Tag the tokens in a string.
--
-- Returns a space-separated string of tokens, each token suffixed
-- with the part of speech.  For example:
--
-- > tag tagger "the dog jumped ."
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