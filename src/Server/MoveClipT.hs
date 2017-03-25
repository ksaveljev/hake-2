{-# LANGUAGE TemplateHaskell #-}
module Server.MoveClipT
    ( module Server.MoveClipT
    ) where

import           Control.Lens (makeLenses)
import           Linear       (V3(..))

import           Game.TraceT  (newTraceT)
import           Types

makeLenses ''MoveClipT

newMoveClipT :: MoveClipT
newMoveClipT = MoveClipT
    { _mcBoxMins     = V3 0 0 0
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