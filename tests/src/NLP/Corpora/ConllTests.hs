module NLP.Corpora.ConllTests where

import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty (TestTree, testGroup)

import qualified NLP.Corpora.Conll as C
import NLP.Types

tests :: TestTree
tests = testGroup "NLP.Corpora.Conll"
        [ testProperty "POS Tags round-trip" prop_tagsRoundTrip
        , testProperty "Chunk Tags round-trip" prop_chunkTagsRoundTrip
        , testProperty "NER Tags round-trip" prop_nerTagsRoundTrip
        ]

prop_tagsRoundTrip :: C.Tag -> Bool
prop_tagsRoundTrip tag = tag == (parseTag . fromTag) tag

prop_nerTagsRoundTrip :: C.NERTag -> Bool
prop_nerTagsRoundTrip tag = tag == (fromRight . parseNERTag . fromNERTag) tag

prop_chunkTagsRoundTrip :: C.Chunk -> Bool
prop_chunkTagsRoundTrip tag = tag == (fromRight . parseChunk . fromChunk) tag

fromRight :: Either a b -> b
fromRight (Left  _) = error "Expected a 'Right' value"
fromRight (Right v) = v
