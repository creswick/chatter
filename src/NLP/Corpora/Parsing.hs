{-# LANGUAGE OverloadedStrings #-}
module NLP.Corpora.Parsing where

import qualified Data.Text as T
import Data.Text (Text)

import Text.Read (readMaybe)

import NLP.Types (POSTag(..))

-- | Read a POS-tagged corpus out of a Text string of the form:
-- "token/tag token/tag..."
--
-- > readPOS "Dear/jj Sirs/nns :/: Let/vb"
-- [("Dear",JJ),("Sirs",NNS),(":",Other ":"),("Let",VB)]
readPOS :: Text -> [(Text, POSTag)]
readPOS str = map toTagged $ T.words str
    where
      toTagged :: Text -> (Text, POSTag)
      toTagged txt = let
        (tok, tagStr) = T.breakOnEnd "/" (T.strip txt)
        in (safeInit tok, parseTag tagStr)

-- | Parse a text string into a POSTag.  This assumes input that is
-- amenable to 'read'.  For example:
--
-- > parseTag "JJ"
-- JJ
--
-- > parseTag "WP$"
-- WPS
--
-- > parseTag "NP-S"
-- Other "NP-S"
--
-- > parseTag "VBX-$"
-- Other "VBX-$"
parseTag :: Text -> POSTag
parseTag txt | "$" `T.isInfixOf` txt = parseTag $ T.replace "$" "S" txt
             | otherwise = case readMaybe (T.unpack $ T.toUpper txt) of
                Just tag -> tag
                Nothing  -> Other txt

-- | Returns all but the last element of a string, unless the string
-- is empty, in which case it returns that string.
safeInit :: Text -> Text
safeInit str | T.length str == 0 = str
             | otherwise         = T.init str