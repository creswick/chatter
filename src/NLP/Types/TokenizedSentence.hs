{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module NLP.Types.TokenizedSentence

where

import GHC.Generics
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Serialize (Serialize)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Safe (headMay, lastMay)
import Text.Read (readEither)

import Text.PrettyPrint (hsep, text)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint.HughesPJClass as HPJ
import Test.QuickCheck (Arbitrary(..), NonEmptyList(..))
import Test.QuickCheck.Instances ()

import NLP.Types.General
import NLP.Types.Annotations
import NLP.Types.Classes

-- | Wrapper around both the underlying text and the tokenizer results.
data TokenizedSentence =
  TokenizedSentence { tokText :: Text
                    , tokAnnotations :: [Annotation Text Token]
                    } deriving (Read, Show, Eq, Generic, Ord)

-- | Get the raw tokens out of a 'TokenizedSentence'
tokens :: TokenizedSentence -> [Token]
tokens ts = map value $ tokAnnotations ts

toTextToks :: TokenizedSentence -> [Text]
toTextToks ts = map getText $ tokAnnotations ts

instance Hashable TokenizedSentence

instance AnnotatedText TokenizedSentence where
  getText = tokText

instance Pretty TokenizedSentence where
  -- TODO shouldn't need the unpack here:
  pPrint tokSent = text (T.unpack $ foldl' fn "" (tokAnnotations tokSent))
    where
      fn :: Text -> Annotation Text Token-> Text
      fn acc ann = let acc' | T.length acc == 0 = acc
                            | otherwise         = T.append acc " "
                   in T.append acc' (getText ann)


-- | TODO see if we can get rid of this..
-- instance AnnotatedText (Annotation TokenizedSentence pos) where
--   getText ann = let tokens = tokAnnotations (payload ann)
--                     text = tokText (payload ann)
--                     startTok = tokens !! (fromIndex $ startIdx ann)
--                     endTok = tokens !! (len ann + (fromIndex $ startIdx ann) - 1)
--                     start = fromIndex $ startIdx startTok
--                 in T.take (len endTok) (T.drop start $ text)


-- | Sentinel value for tokens.
newtype Token = Token Text
  deriving (Read, Show, Eq, Hashable, Ord)

instance Pretty Token where
  pPrint (Token t) = text (T.unpack t)

instance AnnotatedText Token where
  getText (Token txt) = txt

instance Arbitrary Token where
  arbitrary = do NonEmpty txt <- arbitrary
                 return $ Token (T.pack txt)

instance IsString Token where
  fromString = Token . T.pack

instance HasMarkup Token where
  getMarkup ann = ("","")

-- | Extract the last three characters of a 'Token', if the token is
-- long enough, otherwise returns the full token text.
suffix :: Token -> Text
suffix (Token str) | T.length str <= 3 = str
                   | otherwise         = T.drop (T.length str - 3) str

