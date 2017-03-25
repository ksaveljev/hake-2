{-# LANGUAGE TemplateHaskell #-}
module QCommon.NetChannelGlobals
    ( module QCommon.NetChannelGlobals
    ) where

import           Control.Lens (makeLenses)

import           QCommon.SizeBufT (newSizeBufT)
import           Types

makeLenses ''NetChannelGlobals

initialNetChannelGlobals :: NetChannelGlobals
initialNetChannelGlobals = NetChannelGlobals
    { _ncSend = newSizeBufT
    }