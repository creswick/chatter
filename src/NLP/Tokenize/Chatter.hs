module NLP.Tokenize.Chatter
  ( runTokenizer
  , tokenize
  )
where

import Data.Text (Text)
import NLP.Tokenize.Text (Tokenizer, defaultTokenizer, run)
import NLP.Types.Tree

tokenize :: Text -> Sentence
tokenize txt = runTokenizer defaultTokenizer txt

runTokenizer :: Tokenizer -> (Text -> Sentence)
runTokenizer tok txt = Sent $ map Token (run tok txt)
