module NLP.Corpora.BrownTests where

import Test.Framework ( testGroup, Test )
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified NLP.Corpora.Brown as B
import NLP.Types

tests :: Test
tests = testGroup "NLP.Corpora.Brown"
        [ testProperty "Brown POS Tags round-trip" prop_tagsRoundTrip
        ]

prop_tagsRoundTrip :: B.Tag -> Bool
prop_tagsRoundTrip tag = tag == (parseTag . fromTag) tag

