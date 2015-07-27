{-# LANGUAGE OverloadedStrings #-}
module NLP.Corpora.Parsing where

import qualified Data.Text as T
import Data.Text (Text)

import NLP.Types.Tree (Tag(..), parseTag, tagUNK, TaggedSentence(..)
                 , POS(..), Token(..))

-- | Read a POS-tagged corpus out of a Text string of the form:
-- "token\/tag token\/tag..."
--
-- >>> readPOS "Dear/jj Sirs/nns :/: Let/vb"
-- [("Dear",JJ),("Sirs",NNS),(":",Other ":"),("Let",VB)]
--
readPOS :: Tag t => Text -> TaggedSentence t
readPOS str = readPOSWith parseTag str

readPOSWith :: Tag t => (Text -> t) -> Text -> TaggedSentence t
readPOSWith parser str = TaggedSent $ map toTagged $ T.words str
    where
      toTagged txt | "/" `T.isInfixOf` txt = let
          (tok, tagStr) = T.breakOnEnd "/" (T.strip txt)
          in POS (parser tagStr) (Token $ safeInit tok)
                   | otherwise = POS tagUNK (Token txt)

-- | Returns all but the last element of a string, unless the string
-- is empty, in which case it returns that string.
safeInit :: Text -> Text
safeInit str | T.length str == 0 = str
             | otherwise         = T.init str
