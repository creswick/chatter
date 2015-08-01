module NLP.Types.Arbitrary where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Gen (Gen(..), infiniteListOf, elements)

import NLP.Types
import NLP.Tokenize (tokenize)


arbitraryText :: Gen Text
arbitraryText = do
  let chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++
              "!@#$%^&*()}{][-`~',.\"<>?+|;:"
  str <- infiniteListOf $ elements chars
  size <- arbitrary
  return $ T.pack $ take size str

instance Arbitrary TokenizedSentence where
  arbitrary = do
    txt <- arbitraryText
    return $ tokenize txt

instance Arbitrary pos => Arbitrary (TaggedSentence pos) where
  arbitrary = do
    tsent <- arbitrary
    posTags <- infiniteListOf arbitrary
    let annotations = zipWith mkAnnotation
                        [1..(length $ tokAnnotations tsent)]
                        posTags
        mkAnnotation idx pos = Annotation { startIdx = Index idx
                                          , len = 1
                                          , value = pos
                                          , payload = tsent
                                          }
    return TaggedSentence { tagTokSentence = tsent
                          , tagAnnotations = annotations }
