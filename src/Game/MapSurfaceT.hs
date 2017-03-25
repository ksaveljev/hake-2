{-# LANGUAGE TemplateHaskell #-}
module Game.MapSurfaceT
    ( module Game.MapSurfaceT
    ) where

import           Control.Lens   (makeLenses)

import           Game.CSurfaceT (newCSurfaceT)
import           Types

makeLenses ''MapSurfaceT

newMapSurfaceT :: MapSurfaceT
newMapSurfaceT = MapSurfaceT newCSurfaceT Nothing