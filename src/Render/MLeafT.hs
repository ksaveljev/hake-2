{-# LANGUAGE TemplateHaskell #-}
module Render.MLeafT where

import Data.Sequence (Seq)
import Control.Lens (makeLenses)

import Render.MSurfaceT

data MLeafT =
  MLeafT { _mlCluster         :: Int
         , _mlArea            :: Int
         , _mlNumMarkSurfaces :: Int
         , _mlMarkIndex       :: Int
         , _mlMarkSurfaces    :: Seq MSurfaceT
         }

makeLenses ''MLeafT
