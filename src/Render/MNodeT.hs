{-# LANGUAGE TemplateHaskell #-}
module Render.MNodeT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

import Game.CPlaneT

data MNodeT =
  MNodeT { _mnContents     :: Int
         , _mnVisFrame     :: Int
         , _mnMins         :: V3 Float
         , _mnMaxs         :: V3 Float
         , _mnParent       :: MNodeT
         , _mnPlane        :: CPlaneT
         , _mnChildren     :: (MNodeT, MNodeT)
         , _mnFirstSurface :: Int
         , _mnNumSurfaces  :: Int
         }

makeLenses ''MNodeT

newMNodeT :: MNodeT
newMNodeT = undefined -- TODO
