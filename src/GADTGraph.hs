{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module GADTGraph where

import Data.HList
import Data.Text (Text)
import qualified Data.Text as T

import NLP.POS
import NLP.Types
import NLP.Tokenize.Chatter
import qualified NLP.Corpora.Conll as C

--
-- This does hide the arcs type, but also places strict restrictions
-- on what can be done with the val type. (it is /only/ Show-able)
--
-- data Node val where
--   Node :: (Show val, Show outArcs) => val -> outArcs -> Node payload
--
-- deriving instance Show (Node val)


data Node val arcs = Start arcs
                   | Node val arcs
                   deriving (Read, Show)


