{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.ClientT ( ClientT(..)
                      , module Server.ClientT
                      , module Game.UserCmdT
                      , module Game.EdictT
                      , module QCommon.SizeBufT
                      , module QCommon.NetChanT
                      ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Internal
import Game.UserCmdT
import Game.EdictT
import QCommon.SizeBufT
import QCommon.NetChanT

makeLenses ''ClientT

newClientT :: ClientT
newClientT =
  ClientT { _cState         = 0
          , _cUserInfo      = ""
          , _cLastFrame     = 0
          , _cLastCmd       = newUserCmdT
          , _cCommandMsec   = 0
          , _cFrameLatency  = UV.empty -- TODO
          , _cPing          = 0
          , _cMessageSize   = UV.empty -- TODO
          , _cRate          = 0
          , _cSurpressCount = 0
          , _cEdict         = newEdictT
          , _cName          = ""
          , _cMessageLevel  = 0
          , _cDatagram      = newSizeBufT
          , _cDatagramBuf   = ""
          , _cFrames        = V.empty -- TODO
          , _cDownload      = ""
          , _cDownloadSize  = 0
          , _cDownloadCount = 0
          , _cLastMessage   = 0
          , _cLastConnect   = 0
          , _cChallenge     = 0
          , _cNetChan       = newNetChanT
          , _cServerIndex   = 0
          }
