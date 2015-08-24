{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
module Data.TagTree

where
-- import           Prelude
import qualified Data.ListLike as LL
import qualified Data.List as DL (foldl')
import           Data.Monoid ((<>))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T


-- | Annotations are the base of all tags (POS tags, Chunks, marked
-- entities, etc.)
--
-- The semantics of the particular annotation depend on the type of
-- the value, and these can be wrapped up in a newtype for easier use.
data Annotation dat tag =
  Annotation { startIdx :: {-# UNPACK #-} !Int
             -- ^ The starting index of the annotation (a character
             -- offset into the underlying data).
             , len :: {-# UNPACK #-} !Int
             -- ^ The end index of the annotation.
             , value :: tag
             -- ^ The value, such as a POS tag.
             , payload :: dat
             -- ^ The underlying thing that is being annotated (such
             -- as a text string, or a list of other annotations)
             } deriving (Read, Show, Eq, Ord)

class HasMarkup a where
  getMarkup :: a -> (String, String)

instance HasMarkup (Annotation dat tag) where
  getMarkup ann = ("[","]")

data TagTree dat ann where
  Raw :: dat -> TagTree dat ()
  Leaf :: dat -> [Annotation dat ann] -> TagTree dat ann
  Tree :: ( ToString item2, LL.ListLike dat2 item2, LL.FoldableLL dat2 item2)
       => (TagTree dat2 ann2) -> [Annotation (TagTree dat2 ann2) ann] -> TagTree (TagTree dat2 ann2) ann

-- data TagTree dat ann = Leaf dat [Annotation dat ann]
--                      | Tree (TagTree dat ann2) [Annotation (TagTree dat ann2) ann]

type TokenizedSentence = TagTree Text Token
type TaggedSentence = TagTree (TagTree Text Token) POS
type ChunkedSentence = TagTree (TagTree (TagTree Text Token) POS) Chunk


getAnnotations :: TagTree dat ann -> [Annotation dat ann]
getAnnotations (Leaf _ anns) = anns
getAnnotations (Tree _ anns) = anns

getData :: TagTree dat ann -> dat
getData (Leaf dat _) = dat
getData (Tree dat _) = dat

display :: (LL.ListLike dat item, LL.FoldableLL dat item, ToString item)
        => TagTree dat ann
        -> String
display (Tree dat anns) = displayHelper (makeInsertionMap anns) dat
display (Leaf dat anns) = displayHelper Map.empty (Leaf dat anns)

displayHelper :: (LL.ListLike dat item, LL.FoldableLL dat item, ToString item)
        => Map Int String
        -> TagTree dat ann
        -> String
displayHelper insertions (Tree dat anns) = displayHelper (makeInsertionMap anns) dat
displayHelper insertions (Leaf dat anns) = let (_, folded) = LL.foldl' fn (0,"") dat
                                           in case Map.lookup (LL.length dat) insertions of
                                                Nothing -> reverse folded
                                                Just  m -> reverse ((reverse m) <> folded)
  where
    fn :: (ToString item) => (Int, String) -> item -> (Int, String)
    fn (idx, acc) ch = let newIdx = idx + 1
                           markedAcc = case Map.lookup idx insertions of
                                      Nothing -> acc
                                      Just m  -> (reverse m) <>acc
                       in (newIdx, (reverse $ toString ch) <> markedAcc)

makeInsertionMap :: [Annotation dat ann] -> Map Int String
makeInsertionMap anns = LL.foldl' mkInsertions Map.empty $ map project anns

mkInsertions :: Map Int String -> Annotation dat ann -> Map Int String
mkInsertions theMap ann = let (pfx, sfx) = getMarkup ann
                              sidx = startIdx ann
                              eidx = sidx + len ann
                          in Map.insertWith (\new old -> new <> old) eidx sfx
                               (Map.insertWith (\new old -> old <> new) sidx pfx theMap)

-- | Project an annotation all the way onto the underlying data.
project :: Annotation (TagTree dat tag) val -> Annotation dat tag
project an@(Annotation _ _ _ (Leaf _ _)) = project1 an
project an@(Annotation _ _ _ (Tree _ _)) = project (project1 an)

-- | Adjust an annotation such that it is in terms of the next level of annotation.
project1 :: Annotation (TagTree dat tag) val -> Annotation dat val
project1 (Annotation s l tag tree) = let anns = getAnnotations tree
                                         newDat = getData tree
                                         newS = startIdx (anns!!s)

                                         getEndIndex :: Annotation a b -> Int
                                         getEndIndex a = (startIdx a) + (len a)

                                         newL = (getEndIndex (anns!!(s+l))) - newS
                                     in Annotation newS newL tag newDat


class ToString a where
  toString :: a -> String

instance ToString elt => ToString [elt] where
  toString = LL.foldl' (\acc elt -> acc <> toString elt)  ""

instance ToString String where
  toString = id

instance ToString Char where
  toString c = c:""

instance ToString Text where
  toString = T.unpack
