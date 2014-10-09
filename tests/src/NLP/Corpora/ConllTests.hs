module NLP.Corpora.ConllTests where

import Test.Framework ( testGroup, Test )
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified NLP.Corpora.Conll as C
import NLP.Types

tests :: Test
tests = testGroup "NLP.Corpora.Conll"
        [ testProperty "POS Tags round-trip" prop_tagsRoundTrip
        ]

prop_tagsRoundTrip :: C.Tag -> Bool
prop_tagsRoundTrip tag = tag == (parseTag . fromTag) tag

