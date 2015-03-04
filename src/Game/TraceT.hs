{-# LANGUAGE TemplateHaskell #-}
module Game.TraceT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

import Game.CPlaneT
import Game.CSurfaceT
import Game.EdictT

data TraceT =
  TraceT { _tAllSolid   :: Bool
         , _tStartSolid :: Bool
         , _tFraction   :: Float
         , _tEndPos     :: V3 Float
         , _tPlane      :: CPlaneT
         , _tSurface    :: CSurfaceT
         , _tContents   :: Int
         , _tEnt        :: EdictT
         }

makeLenses ''TraceT
