{-# LANGUAGE TemplateHaskell #-}
module Render.MEdgeT where

import Data.Word (Word16)
import Control.Lens (makeLenses)

data MEdgeT =
  MEdgeT { _meV                :: (Word16, Word16)
         , _meCachedEdgeOffset :: Int
         }

makeLenses ''MEdgeT
