{-# LANGUAGE TemplateHaskell #-}
module Sys.NETGlobals ( module Sys.NETGlobals
                      , module Sys.LoopbackT
                      ) where

import Control.Lens (makeLenses)

import Internal
import QCommon.NetAdrT
import Sys.LoopbackT

makeLenses ''NETGlobals

initialNETGlobals :: NETGlobals
initialNETGlobals =
  NETGlobals { _ngLoopbacks   = (newLoopbackT, newLoopbackT)
             , _ngIpSockets   = (Nothing, Nothing)
             , _ngNetLocalAdr = newNetAdrT
             }
