module NLP.Tokenize.Types where

import Data.Text (Text)

data RawToken = FixedToken { start :: Int
                           , text :: Text
                           }
              | OpenToken  { start :: Int
                           , text :: Text
                           }
                deriving (Read, Show)
