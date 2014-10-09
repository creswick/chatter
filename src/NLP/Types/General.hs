{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Types.General
where

import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics

import Test.QuickCheck (Arbitrary(..), elements)

-- | Just a handy alias for Text
type Error = Text

-- | Boolean type to indicate case sensitivity for textual
-- comparisons.
data CaseSensitive = Sensitive | Insensitive
  deriving (Read, Show, Generic)

instance Serialize CaseSensitive
instance Arbitrary CaseSensitive where
  arbitrary = elements [Sensitive, Insensitive]
