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
prop_tagsRoundTrip tag = tag == (safeParsePOS . serializePOS) tag

prop_nerTagsRoundTrip :: C.NERTag -> Bool
prop_nerTagsRoundTrip tag = tag == (fromRight . parseNETag . serializeNETag) tag

prop_chunkTagsRoundTrip :: C.Chunk -> Bool
prop_chunkTagsRoundTrip tag = tag == (fromRight . parseChunk . serializeChunk) tag

fromRight :: Either a b -> b
fromRight (Left  _) = error "Expected a 'Right' value"
fromRight (Right v) = v
