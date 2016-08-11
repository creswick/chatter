{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module NLP.Types.ChunkedSentence

where

import GHC.Generics
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.List (foldl', group)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Serialize (Serialize)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Safe (headMay, lastMay)
import Text.Read (readEither)

import Text.PrettyPrint (hsep, text)
import Text.PrettyPrint.HughesPJClass (Pretty(..))
import qualified Text.PrettyPrint.HughesPJClass as HPJ
import Test.QuickCheck (Arbitrary(..), NonEmptyList(..))
import Test.QuickCheck.Instances ()

import NLP.Types.General
import NLP.Types.Annotations
import NLP.Types.TokenizedSentence
import NLP.Types.TaggedSentence
import NLP.Types.Classes
import NLP.Parsing.ChunkedSentenceParser
import NLP.Parsing.ChunkedSentenceScanner

parseChunkedSentence :: (POS pos, Chunk chunk) => Text -> Either Error (ChunkedSentence pos chunk)
parseChunkedSentence txt = let cs = parse $ alexScanTokens $ T.unpack txt
--                                naturalText = T.intercalate (" ") (getNaturalText cs)
                           in parseCS txt cs

parseCS :: (POS pos, Chunk chunk) => Text -> CS -> Either Error (ChunkedSentence pos chunk)
parseCS inText sent@(CS cocs) = do
  taggedSent <- parseTS inText sent
  theChunkAnnotations <- chunkedAnnotations taggedSent cocs
  return $ ChunkedSentence { chunkTagSentence = taggedSent
                           , chunkAnnotations = theChunkAnnotations
                           }

  where
    chunkedAnnotations :: (POS pos, Chunk chunk)
                          => TaggedSentence pos
                          -> [ChunkOrChink]
                          -> Either Error [Annotation (TaggedSentence pos) chunk]
    chunkedAnnotations tagSent cocs = let (_, result) = foldl' (foldFn tagSent) (0, Right []) cocs
                                      in reverse `fmap` result

    foldFn :: (POS pos, Chunk chunk)
              => TaggedSentence pos
              -> (Int, Either Error [Annotation (TaggedSentence pos) chunk])
              -> ChunkOrChink
              -> (Int, Either Error [Annotation (TaggedSentence pos) chunk])
    foldFn       _ (idx, Left err)     _ = (idx, Left err)
    foldFn tagSent (idx, Right anns) coc =
      let nextIdx = idx + tokLength coc
          annLength = tokLength coc
          result = case coc of
                     (Chink     _) -> Right anns
                     (Chunk lex _) -> (\a -> Right (a:anns)) =<< annotation lex

          annotation lex = do
            theChunk <- parseChunk =<< lexChunkTag lex
            return Annotation { startIdx = Index idx
                              , len = annLength
                              , value = theChunk
                              , payload = tagSent
                              }

      in (nextIdx, result)

parseTS :: (POS pos) => Text -> CS -> Either Error (TaggedSentence pos)
parseTS inText cs = do
  let ptoks = getPOSToks cs
  theTokAnnotations <- sequence (map toTokAnnotation ptoks)
  let tokSent = TokenizedSentence { tokText = inText
                                  , tokAnnotations = theTokAnnotations
                                  }
  tAnnotations <- taggedAnnotations tokSent ptoks
  return TaggedSentence { tagTokSentence = tokSent
                        , tagAnnotations = tAnnotations
                        }
  where
    toTokAnnotation :: PosTok -> Either Error (Annotation Text Token)
    toTokAnnotation pt@(PosTok {..}) = do
      theValue <- ptTokText pt
      return Annotation { startIdx = Index ptLoc -- wrong! counts the raw input, with tag & chunk markings.
                        , len = T.length theValue
                        , value = Token theValue
                        , payload = inText
                        }

    taggedAnnotations :: POS pos
                         => TokenizedSentence
                         -> [PosTok]
                         -> Either Error [Annotation TokenizedSentence pos]
    taggedAnnotations tokSent ptoks = let (_, result) = foldl' (foldFn tokSent) (0, Right []) ptoks
                                      in reverse `fmap` result

    foldFn :: POS pos
           => TokenizedSentence
           -> (Int, Either Error ([Annotation TokenizedSentence pos]))
           -> PosTok
           -> (Int, Either Error ([Annotation TokenizedSentence pos]))
    foldFn       _ (idx , Left err)     _ = (idx + 1, Left err)
    foldFn tokSent (idx, Right anns) ptok = case (toTagAnnotation tokSent idx ptok) of
      Left  err -> (idx + 1, Left err)
      Right ann -> (idx + 1, Right (ann:anns))

    -- Need the token position
    toTagAnnotation :: POS pos
                       => TokenizedSentence
                       -> Int
                       -> PosTok
                       -> Either Error (Annotation TokenizedSentence pos)
    toTagAnnotation tokSent tokIdx pt@(PosTok {..}) = do
      thePos <- parsePOS =<< (ptPosText pt)
      return Annotation { startIdx = Index tokIdx
                        , len = 1
                        , value = thePos
                        , payload = tokSent
                        }

-- | A 'Chunked' sentence, with underlying Part-of-Speech tags and tokens.
-- Note: This is not a deep tree, a separate parse tree is needed.
data ChunkedSentence pos chunk =
  ChunkedSentence { chunkTagSentence :: TaggedSentence pos
                  , chunkAnnotations :: [Annotation (TaggedSentence pos) chunk]
                  } deriving (Read, Show, Eq, Generic, Ord)

instance (Hashable pos, Hashable chunk) => Hashable (ChunkedSentence pos chunk)

-- TODO rewrite per. tagInsertions.
chunkInsertions :: (Chunk chunk, HasMarkup chunk, POS pos, HasMarkup pos)
                => Map Int Text -> [Annotation (TaggedSentence pos) chunk] -> Map Int Text
chunkInsertions initMap anns = foldl' mkInsertions initMap anns
  where
    mkInsertions :: (HasMarkup pos, HasMarkup chunk)
                 => Map Int Text -> Annotation (TaggedSentence pos) chunk -> Map Int Text
    mkInsertions theMap ann@(Annotation (Index startIdx) annLen chunk (TaggedSentence tokSent tags)) =
      let (pfx, sfx) = getAnnotationMarkup ann
          -- Get the text for the first token. (TODO: do we need the pretty text here?)
          startTok = getText (tags !! startIdx)

          endIdx = (startIdx + annLen) - 1
          endTok = getText (tags !! endIdx)

      -- These insertWith's bear a bit of explaining:
      --   They insert a full token (prefix + token) or (token + suffix), if nothing
      -- is in the map at that index yet.  If there *is* something there, then the token
      -- part doesn't need repeating, so we just prepend or append the prefix or suffix.
      in Map.insertWith (\_ old -> (T.pack pfx) <> old) startIdx (T.pack pfx <> startTok)
           (Map.insertWith (\_ old -> old <> (T.pack sfx)) endIdx (endTok <> T.pack sfx) theMap)


-- | Build a ChunkedSentence from a list of chunks and a corresponding
-- TaggedSentence.  This is not quite like the TaggedSentence version
-- ('applyTags') because consequetive equal chunks denote branching in
-- the tree.  Also, note that this is lossy; consecutive chunk tags
-- *will* be collapsed when creating a 'ChunkedSentence' this way.
toChunkedSentence :: (Chunk chunk, POS tag) => TaggedSentence tag -> [chunk] -> ChunkedSentence tag chunk
toChunkedSentence taggedSentence chunks =
  let groups = map (\g -> (head g, length g)) $ group chunks

      mkAnnotation (idx, acc) (chunk, chunkLen) | chunk == notChunk = (idx + chunkLen, acc)
                                                | otherwise         =
        let ann = Annotation { startIdx = Index idx
                             , len = chunkLen
                             , value = chunk
                             , payload = taggedSentence
                             }
        in ( idx + chunkLen, ann:acc )

  in ChunkedSentence
       { chunkTagSentence = taggedSentence
       , chunkAnnotations = reverse $ snd $ foldl' mkAnnotation (0,[]) groups
       }

-- | The dual of 'toChunkedSentence'.
--
-- This takes a 'ChunkedSentence' and removes the chunks, returning
-- the underlying tagged sentence paired with a list of parallel chunk
-- tags that apply to each POS tag in the 'TaggedSentence'.
fromChunkedSentence :: (Chunk chunk, POS pos)
                    => ChunkedSentence pos chunk
                    -> (TaggedSentence pos, [chunk])
fromChunkedSentence chunkedSent =
  let taggedSent = chunkTagSentence chunkedSent

      chunks = let (lastIdx, anns) = foldl fn (0,[]) (chunkAnnotations chunkedSent)
                   missingOs = (tsLength taggedSent) - 1 - lastIdx
                   lastOs = replicate missingOs notChunk
               in reverse (lastOs ++ anns)

      fn :: (Chunk chunk, POS pos) => (Int, [chunk]) -> Annotation (TaggedSentence pos) chunk -> (Int, [chunk])
      fn (idx, acc) ann =
        let outChunks = replicate ((fromIndex $ startIdx ann) - idx) notChunk
            newChunks = replicate (len ann) (value ann)
            newIdx = (fromIndex $ startIdx ann) + len ann

        in (newIdx, newChunks ++ outChunks ++ acc)

  in (taggedSent, chunks)

instance AnnotatedText (ChunkedSentence pos chunk) where
  getText = getText . chunkTagSentence

-- | Create an annotation that is over a lower-level version of a sequence of annotations.
projectTagged :: Annotation (TaggedSentence pos) ann -> [Annotation TokenizedSentence ann]
projectTagged ann =
  let tokens = tagAnnotations $ payload ann

      -- The index of the first (Annotation TokenizedSentence ann):
      startTokIdx = fromIndex $ startIdx ann
      -- the list of tokens that this annotation ranges over:
      toks = take (len ann) $ drop startTokIdx $ tagAnnotations $ payload ann

      newAnnotation tokAnn = tokAnn { value = value ann }
  in map newAnnotation toks


instance AnnotatedText (Annotation (TaggedSentence pos) chunk) where
  getText theAnn = T.intercalate " " $ map getText $ projectTagged theAnn

instance (Chunk chunk, POS pos) => Pretty (ChunkedSentence pos chunk) where
  pPrint cs = text (T.unpack toStr)
    where
      tokenAnnotations = tokAnnotations $ tagTokSentence $ chunkTagSentence cs

      toStr :: Text
      toStr = T.intercalate " " $ zipWith pickEntry (map getText tokenAnnotations) [0..]

      pickEntry :: Text -> Int -> Text
      pickEntry txt idx = case Map.lookup idx insertions of
                            Nothing -> txt
                            Just  t -> t

      tagMap = tagInsertions Map.empty (tagAnnotations $ chunkTagSentence cs)
      insertions = chunkInsertions tagMap $ chunkAnnotations cs
