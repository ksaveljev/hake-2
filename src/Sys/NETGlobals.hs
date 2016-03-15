{-# LANGUAGE TemplateHaskell #-}
module Sys.NETGlobals
  ( module Sys.NETGlobals
  ) where

import QCommon.SizeBufT (newSizeBufT)
import Sys.LoopbackT (newLoopbackT)
import Types

import Control.Lens (makeLenses)

makeLenses ''NETGlobals

initialNETGlobals :: NETGlobals
initialNETGlobals =
  NETGlobals { _ngLoopbackClient = newLoopbackT
             , _ngLoopbackServer = newLoopbackT
             , _ngIpSocketClient = Nothing
             , _ngIpSocketServer = Nothing
             , _ngSend           = newSizeBufT
             }
