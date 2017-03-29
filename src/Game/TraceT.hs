{-# LANGUAGE TemplateHaskell #-}
module Game.TraceT ( TraceT(..)
                   , module Game.TraceT
                   ) where

import Control.Lens (makeLenses)
import Linear (V3(..))

import Types
import Game.CPlaneT

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
