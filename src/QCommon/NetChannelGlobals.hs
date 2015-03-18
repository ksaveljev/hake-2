{-# LANGUAGE TemplateHaskell #-}
module QCommon.NetChannelGlobals where

import Control.Lens (makeLenses)

import Internal

makeLenses ''NetChannelGlobals

initialNetChannelGlobals :: NetChannelGlobals
initialNetChannelGlobals =
  NetChannelGlobals {
                    }
