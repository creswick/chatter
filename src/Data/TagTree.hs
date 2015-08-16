{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
module Data.TagTree

where
import           Prelude hiding (reverse, length)
import           Data.ListLike hiding (toString)
import qualified Data.List as DL (foldl')
import           Data.Monoid ((<>))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text hiding (reverse, length, foldl')

-- | Safe index type, uses a phantom type to prevent us from indexing
-- into the wrong thing.
newtype Index a = Index Int
  deriving (Read, Show, Eq, Ord)

-- | Annotations are the base of all tags (POS tags, Chunks, marked
-- entities, etc.)
--
-- The semantics of the particular annotation depend on the type of
-- the value, and these can be wrapped up in a newtype for easier use.
data Annotation dat tag =
  Annotation { startIdx :: {-# UNPACK #-} !(Index dat)
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
  Leaf :: dat -> [Annotation dat ann] -> TagTree dat ann
  Tree :: (TagTree dat2 ann2) -> [Annotation (TagTree dat2 ann2) ann] -> TagTree (TagTree dat2 ann2) ann

class ToString a where
  toString :: a -> String

instance ToString elt => ToString [elt] where
  toString = foldl' (\acc elt -> acc <> toString elt)  ""

instance ToString String where
  toString = id

instance ToString Char where
  toString c = c:""

instance ToString Text where
  toString = unpack

display :: (ListLike dat item, FoldableLL dat item, Show dat, ToString item)
        => TagTree dat ann
        -> String
-- display (Tree dat anns) = 
display (Leaf dat anns) = let (_, folded) = foldl' fn (0,"") dat
                          in case Map.lookup (length dat) insertions of
                               Nothing -> reverse folded
                               Just  m -> reverse ((reverse m) <> folded)
  where
    fn :: (ToString item) => (Int, String) -> item -> (Int, String)
    fn (idx, acc) ch = let newIdx = idx + 1
                           markedAcc = case Map.lookup idx insertions of
                                      Nothing -> acc
                                      Just m  -> (reverse m) <>acc
                       in (newIdx, (reverse $ toString ch) <> markedAcc)

    insertions :: Map Int String
    insertions = DL.foldl' mkInsertions (Map.empty) anns

    mkInsertions :: Map Int String -> Annotation dat ann -> Map Int String
    mkInsertions theMap ann = let (pfx, sfx) = getMarkup ann
                                  Index sidx = startIdx ann
                                  eidx = sidx + len ann
                              in Map.insertWith (<>) eidx sfx (Map.insertWith (<>) sidx pfx theMap)


-- | Project the annotations on something that contains annotations
-- onto the next layer of annotated thing.
--
-- For example: projecting an `Annotation TokenizedSentence RawTag` to
-- be an `Annotation Text RawTag`.
--
-- Project adjusts the index, length, and the payload of an annotation
-- to reflect the lower-level data.
-- project :: Tree dat => Annotation dat tag -> Annotation dat2 tag
-- project ann = undefined
