{-# LANGUAGE TemplateHaskell #-}
module Sys.NETGlobals where

import           Control.Lens     (makeLenses)

import           Types
import           QCommon.SizeBufT
import           Sys.LoopbackT

makeLenses ''NETGlobals

initialNETGlobals :: NETGlobals
initialNETGlobals = NETGlobals
    { _ngLoopbackClient = newLoopbackT
    , _ngLoopbackServer = newLoopbackT
    , _ngIpSocketClient = Nothing
    , _ngIpSocketServer = Nothing
    , _ngSend           = newSizeBufT
    }