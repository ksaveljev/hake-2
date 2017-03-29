{-# LANGUAGE TemplateHaskell #-}
module Game.MapSurfaceT where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B

import           Types
import           Game.CSurfaceT

makeLenses ''MapSurfaceT

newMapSurfaceT :: MapSurfaceT
newMapSurfaceT = MapSurfaceT newCSurfaceT Nothing
