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
      return Annotation { startIdx = Index ptLoc
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

chunkInsertions :: (Chunk chunk, HasMarkup chunk, POS pos, HasMarkup pos)
                => Map Int String -> [Annotation (TaggedSentence pos) chunk] -> Map Int String
chunkInsertions initMap anns = foldl' mkInsertions initMap anns
  where
    mkInsertions :: (HasMarkup pos, HasMarkup chunk)
                 => Map Int String -> Annotation (TaggedSentence pos) chunk -> Map Int String
    mkInsertions theMap ann@(Annotation (Index sIdx) l chunk dat) =
      let (pfx, sfx) = getAnnotationMarkup ann

          -- POS annotations:
          tags = tagAnnotations dat

          -- starting index of the tag annotation that marks this chunk:
          sTagIdx = fromIndex $ startIdx (tags!!sIdx)
          eTag = tags!!(sIdx + len ann - 1) -- -1 to account for length.
          eTagIdx = (fromIndex $ startIdx eTag) + len eTag

          -- Token annotations
          toks :: [Annotation Text Token]
          toks = tokAnnotations $ tagTokSentence dat

          -- starting index of the token.  This is the index into the text string:
          sTokIdx = fromIndex $ startIdx (toks!!sTagIdx)
          eTok = toks!!(sTagIdx + len ann - 1) -- -1 to account for length.
          eTokIdx = (fromIndex $ startIdx eTok) + len eTok

      in Map.insertWith (\new old -> new <> old) eTokIdx sfx
           (Map.insertWith (\new old -> old <> new) sTokIdx pfx theMap)


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


instance (Chunk chunk, POS pos) => Pretty (ChunkedSentence pos chunk) where
  pPrint cs = text toStr
    where
      ts = tokText $ tagTokSentence $ chunkTagSentence cs
      toStr = let (_, folded) = T.foldl' fn (0,"") ts
              in case Map.lookup (T.length ts) insertions of
                   Nothing -> reverse folded
                   Just  m -> reverse ((reverse m) <> folded)

      fn :: (Int, String) -> Char -> (Int, String)
      fn (idx, acc) ch = let newIdx = idx + 1
                             markedAcc = case Map.lookup idx insertions of
                                           Nothing -> acc
                                           Just m  -> (reverse m) <>acc
                         in (newIdx, ch:markedAcc)

      insertions = tagInsertions chunkMap (tagAnnotations $ chunkTagSentence cs)
      chunkMap = chunkInsertions Map.empty $ chunkAnnotations cs
