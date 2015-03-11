{-# LANGUAGE TemplateHaskell #-}
module QCommon.NetChannelGlobals where

import Control.Lens (makeLenses)

import Internal
import Game.CVarT

makeLenses ''NetChannelGlobals

initialNetChannelGlobals :: NetChannelGlobals
initialNetChannelGlobals =
  NetChannelGlobals { _ncShowPackets = newCVarT
                    , _ncShowDrop    = newCVarT
                    , _ncQPort       = newCVarT
                    }
