module NLP.Types.Arbitrary where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Gen (Gen(..), infiniteListOf, elements, suchThat)

import NLP.Types
import NLP.Tokenize (runTokenizer, whitespace)


arbitraryText :: Gen Text
arbitraryText = do
  let chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++
              "!@#$%^&*()}{][-`~',.\"<>?+|;:"
  str <- infiniteListOf $ elements chars
  size <- arbitrary
  return $ T.pack $ take size str

instance Arbitrary RawTag where
  arbitrary = do
    str <- suchThat arbitraryText (\x-> T.length x > 0)
    return $ RawTag str

instance Arbitrary TokenizedSentence where
  arbitrary = do
    txt <- arbitraryText
    return $ runTokenizer whitespace txt

instance Arbitrary pos => Arbitrary (TaggedSentence pos) where
  arbitrary = do
    tsent <- arbitrary
    posTags <- infiniteListOf arbitrary
    let tokCount = length $ tokAnnotations tsent
        annotations = zipWith mkAnnotation
                        [0..]
                        (take tokCount posTags)
        mkAnnotation idx pos = Annotation { startIdx = Index idx
                                          , len = 1
                                          , value = pos
                                          , payload = tsent
                                          }
    return TaggedSentence { tagTokSentence = tsent
                          , tagAnnotations = annotations }

