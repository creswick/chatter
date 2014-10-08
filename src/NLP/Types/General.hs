{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Types.General
where

import Data.Serialize (Serialize, put, get)
import Data.Text (Text)
import GHC.Generics


-- | Just a handy alias for Text
type Error = Text

-- | Boolean type to indicate case sensitivity for textual
-- comparisons.
data CaseSensitive = Sensitive | Insensitive
  deriving (Read, Show, Generic)

instance Serialize CaseSensitive
