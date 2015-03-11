{-# LANGUAGE TemplateHaskell #-}
module Server.MoveClipT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

import Game.EdictT
import Game.TraceT

data MoveClipT =
  MoveClipT { _mcBoxMins     :: V3 Float
            , _mcBoxMaxs     :: V3 Float
            , _mcMins        :: V3 Float -- TODO: are we sure it is V3 ?
            , _mcMaxs        :: V3 Float -- TODO: are we sure it is V3 ?
            , _mcMins2       :: V3 Float
            , _mcMaxs2       :: V3 Float
            , _mcStart       :: V3 Float -- TODO: are we sure it is V3 ?
            , _mcEnd         :: V3 Float -- TODO: are we sure it is V3 ?
            , _mcTrace       :: TraceT
            , _mcPassEdict   :: EdictT
            , _mcContentMask :: Int
            }

makeLenses ''MoveClipT
