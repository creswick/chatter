{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module NLP.Types.General
where

import Data.Serialize (Serialize)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Text.Read (readEither)

import Test.QuickCheck (Arbitrary(..), elements)

-- | Just a handy alias for Text
type Error = Text

toEitherErr :: Either String a -> Either Error a
toEitherErr (Left s) = Left (T.pack s)
toEitherErr (Right r) = Right r

-- | Boolean type to indicate case sensitivity for textual
-- comparisons.
data CaseSensitive = Sensitive | Insensitive
  deriving (Read, Show, Generic)

instance Serialize CaseSensitive
instance Arbitrary CaseSensitive where
  arbitrary = elements [Sensitive, Insensitive]

readEitherVerb :: Read a => Text -> Either Error a
readEitherVerb txt = case readEither $ T.unpack txt of
                       Right val -> Right val
                       Left  err -> Left (T.pack ("Could not parse '"++T.unpack txt++"': "++err))
