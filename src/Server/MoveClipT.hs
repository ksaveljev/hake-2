{-# LANGUAGE TemplateHaskell #-}
module Server.MoveClipT where

import Control.Lens (makeLenses)
import Linear (V3(..))

import QuakeState
import Types

data MoveClipT =
  MoveClipT { _mcBoxMins     :: V3 Float
            , _mcBoxMaxs     :: V3 Float
            , _mcMins        :: V3 Float
            , _mcMaxs        :: V3 Float
            , _mcMins2       :: V3 Float
            , _mcMaxs2       :: V3 Float
            , _mcStart       :: V3 Float
            , _mcEnd         :: V3 Float
            , _mcTrace       :: TraceT
            , _mcPassEdict   :: Maybe (Ref EdictT)
            , _mcContentMask :: Int
            }

makeLenses ''MoveClipT

newMoveClipT :: MoveClipT
newMoveClipT =
  MoveClipT { _mcBoxMins     = V3 0 0 0
            , _mcBoxMaxs     = V3 0 0 0
            , _mcMins        = V3 0 0 0
            , _mcMaxs        = V3 0 0 0
            , _mcMins2       = V3 0 0 0
            , _mcMaxs2       = V3 0 0 0
            , _mcStart       = V3 0 0 0
            , _mcEnd         = V3 0 0 0
            , _mcTrace       = newTraceT
            , _mcPassEdict   = Nothing
            , _mcContentMask = 0
            }
