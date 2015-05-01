{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module QCommon.NetChannelGlobals ( module QCommon.NetChannelGlobals
                                 , module QCommon.SizeBufT
                                 ) where

import Control.Lens (makeLenses)

import Internal
import QCommon.SizeBufT

makeLenses ''NetChannelGlobals

initialNetChannelGlobals :: NetChannelGlobals
initialNetChannelGlobals =
  NetChannelGlobals { _ncSendBuf = "" -- max length Constants.maxMsgLen
                    , _ncSend    = newSizeBufT
                    }
