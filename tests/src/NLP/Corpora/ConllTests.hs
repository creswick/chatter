module NLP.Corpora.ConllTests where

import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty (TestTree, testGroup)

import qualified NLP.Corpora.Conll as C
import NLP.Types

tests :: TestTree
tests = testGroup "NLP.Corpora.Conll"
        [ testProperty "POS Tags round-trip" prop_tagsRoundTrip
        ]

prop_tagsRoundTrip :: C.Tag -> Bool
prop_tagsRoundTrip tag = tag == (parseTag . fromTag) tag

