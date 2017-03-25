{-# LANGUAGE TemplateHaskell #-}
module Sys.NETGlobals
    ( module Sys.NETGlobals
    ) where

import           Control.Lens     (makeLenses)

import           QCommon.SizeBufT (newSizeBufT)
import           Sys.LoopbackT    (newLoopbackT)
import           Types

makeLenses ''NETGlobals

initialNETGlobals :: NETGlobals
initialNETGlobals = NETGlobals
    { _ngLoopbackClient = newLoopbackT
    , _ngLoopbackServer = newLoopbackT
    , _ngIpSocketClient = Nothing
    , _ngIpSocketServer = Nothing
    , _ngSend           = newSizeBufT
    }