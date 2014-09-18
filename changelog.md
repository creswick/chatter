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