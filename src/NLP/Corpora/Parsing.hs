{-# LANGUAGE OverloadedStrings #-}
module NLP.Corpora.Parsing where

import qualified Data.Text as T
import Data.Text (Text)

import NLP.Types (POSTag(..), parseTag)

-- | Read a POS-tagged corpus out of a Text string of the form:
-- "token/tag token/tag..."
--
-- > readPOS "Dear/jj Sirs/nns :/: Let/vb"
-- [("Dear",JJ),("Sirs",NNS),(":",Other ":"),("Let",VB)]
readPOS :: Text -> [(Text, POSTag)]
readPOS str = map toTagged $ T.words str
    where
      toTagged :: Text -> (Text, POSTag)
      toTagged txt | "/" `T.isInfixOf` txt = let
          (tok, tagStr) = T.breakOnEnd "/" (T.strip txt)
          in (safeInit tok, parseTag tagStr)
                   | otherwise = (txt, UNK)

-- | Returns all but the last element of a string, unless the string
-- is empty, in which case it returns that string.
safeInit :: Text -> Text
safeInit str | T.length str == 0 = str
             | otherwise         = T.init str