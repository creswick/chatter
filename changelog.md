= 0.6.0.0 =

 - Switched to using Hashmap for the DefaultMap implementation;
   this *may* break compatibility with old binary stores.

= 0.5.2.1 =

 - Removed an orphan instance for Text.

= 0.5.2.0 =

 - Unceremoniously forced the Chunking model to do basic Named
   Entity Recognition, and added a simple NER model based on Nothman
   et al's WikiNER data set -- which is provided under a very
   permissive CC license.
   (http://schwa.org/projects/resources/wiki/Wikiner)

= 0.5.1.0 =

 - Moved to Tasty from test-framework. I'm treating this as a
   c-level patch because it may have siginficant impact on the
   transitive dependencies.

= 0.5.0.2 =
 - removed upper vesion bounds on many dependencies.

= 0.5.0.1 =

 - Updates for mobx-0.3's changes to use Lazy Text.

= 0.5.0.0 =

 - Added chunk, chunkText, and chunkStr functions to NLP.Chunk to
   make it easier to experiment with the chunker.

 - Added a formatting function to show ChunkedSententences
   (NLP.Types.Tree.showChunkedSent) that formats chunks in bracket notation, eg:

   > chunkText tgr chk "The dog jumped over the reluctant cat."
   "[NP The/DT dog/NN] [VP jumped/VBD] [NP over/IN the/DT reluctant/JJ cat/NN] ./."

   Notice that the features still need some tuning for the chunker.

 - Moved the AveragePerceptron module into a NLP.ML (Machine Learning) module.

 - Added the 'tags-since-dt' feature to the AveragePerceptronChunker features
   and retrained the Conll2000 models.

= 0.4.0.0 =

 - Added phrasal chunking via an Averaged Perceptron Chunker,
   trained on the Conll 2000 corpus.

 - Added a POS tagger trained on the Conll 2000 corpus, because the
   Conll chunker relies on that tagset.

 - Set the Conll 2000 POS tagger to the defaultTagger. Note that
   the tagset is much smaller than the Brown tagset used by the
   previous defaultTagger.  The Brown tagger is still available from
   NLP.POS.brownTagger.

= 0.3.0.1 =

 - Bumped dependency on base to >= 4.6 (from >= 4) because of
   Text.Read.readMaybe.  This drops support for HP2012, and requires
   GHC 7.6 or newer.

 - fixed lots of warnings and added documentation.

= 0.3.0.0 =

 - Changed the Sentence and TaggedSentence data types to be actual
   tree structures with real types at the respective
   layers. ChunkedSentence and ChunkOr were also added to facilitate
   phrase and clause chunking.

 - Added a POS Tag data type for Brown corpus tags, and a Chunk type for
   Chunks as well (in the Brown module, but that's probably not the best
   place, given that the chunks have nothing to do with the actual Brown
   corpus.)

 - Updated the Parsec Examples to use the typed tags/chunks.

 - Regenerated the defaultTager, it uses the Brown tags now instead of
   RawTag.

= 0.2.0.1 =

 - I realized immediately after the 0.2.0.0 release that I broke the
   defaultTagger by adding the protectTerms function to the
   LiteralTagger.  Things broke because (i) there are bugs in that
   functionality, which uses run-time assembled regexes, and (ii) the
   UnambiguousTagger used in the defaultTagger delegates to an instance
   of the LiteralTagger, which pulled in the (semi-broken) protectTerms
   function.  This has been fixed by replacing the tokenizer when the
   LiteralTagger is used as an UnambiguousTagger -- the later tager
   doesn't need the functionality, and it should never have been used
   there anyway.

 - Added a bevy of tests to cover the above fix.

 - Added tests (currently breaking) that exercise the broken bits of
   the protectTerms function.

= 0.2.0.0 =

 - Added support for expressing information extraciton patterns via Parsec.
 - Misc. bug fixes.
