{-# LANGUAGE TemplateHaskell #-}
module Game.TraceT
  ( module Game.TraceT
  ) where

import Game.CPlaneT (newCPlaneT)
import Types

import Control.Lens (makeLenses)
import Linear (V3(..))

makeLenses ''TraceT

newTraceT :: TraceT
newTraceT =
  TraceT { _tAllSolid   = False
         , _tStartSolid = False
         , _tFraction   = 0
         , _tEndPos     = V3 0 0 0
         , _tPlane      = newCPlaneT
         , _tSurface    = Nothing
         , _tContents   = 0
         , _tEnt        = Nothing
         }