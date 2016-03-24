{-# LANGUAGE TemplateHaskell #-}
module QCommon.NetChannelGlobals
  ( module QCommon.NetChannelGlobals
  ) where

import QCommon.SizeBufT (newSizeBufT)
import Types

import Control.Lens (makeLenses)

makeLenses ''NetChannelGlobals

initialNetChannelGlobals :: NetChannelGlobals
initialNetChannelGlobals =
  NetChannelGlobals { _ncSend = newSizeBufT
                    }