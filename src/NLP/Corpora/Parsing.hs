{-# LANGUAGE OverloadedStrings #-}
module NLP.Corpora.Parsing where

import qualified Data.Text as T
import Data.Text (Text)

import NLP.Tokenize
import NLP.Types

-- | Read a POS-tagged corpus out of a Text string of the form:
-- "token\/tag token\/tag..."
--
-- >>> readPOS "Dear/jj Sirs/nns :/: Let/vb"
-- [("Dear",JJ),("Sirs",NNS),(":",Other ":"),("Let",VB)]
--
readPOS :: POS t => Text -> TaggedSentence t
readPOS str = applyTags tokenizedSentence (map snd tagPairs)
    where
      tokenizedSentence = runTokenizer whitespace $ T.unwords $ map fst tagPairs

      tagPairs = map toTagged $ T.words str

-- | Read a 'ChunkedSentence' from a pretty-printed variant.
--
-- The dual of 'prettyShow chunkedSentence'
readChunk :: (Chunk chunk, POS pos) => Text -> ChunkedSentence pos chunk
readChunk str = undefined

-- | Read a standard POS-tagged corpus with one sentence per line, and
-- one POS tag after each token.
readCorpus :: POS pos => Text -> [TaggedSentence pos]
readCorpus corpus = map readPOS $ T.lines corpus

toTagged :: POS pos => Text -> (Text, pos)
toTagged txt | "/" `T.isInfixOf` txt =
               let (tok, tagStr) = T.breakOnEnd "/" (T.strip txt)
               in  (safeInit tok, safeParsePOS tagStr)
             | otherwise = (txt, tagUNK)


-- | Returns all but the last element of a string, unless the string
-- is empty, in which case it returns that string.
safeInit :: Text -> Text
safeInit str | T.length str == 0 = str
             | otherwise         = T.init str
