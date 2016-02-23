{-# LANGUAGE TemplateHaskell #-}
module Game.MapSurfaceT
  ( module Game.MapSurfaceT
  ) where

import Game.CSurfaceT (newCSurfaceT)
import Types

import Control.Lens (makeLenses)

makeLenses ''MapSurfaceT

newMapSurfaceT :: MapSurfaceT
newMapSurfaceT = MapSurfaceT newCSurfaceT Nothing