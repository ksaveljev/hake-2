{-# LANGUAGE TemplateHaskell #-}
module QCommon.NetChannelGlobals where

import           Control.Lens     (makeLenses)

import           QCommon.SizeBufT
import           Types

makeLenses ''NetChannelGlobals

initialNetChannelGlobals :: NetChannelGlobals
initialNetChannelGlobals = NetChannelGlobals
    { _ncSendBuf = "" -- max length Constants.maxMsgLen
    , _ncSend    = newSizeBufT
    }