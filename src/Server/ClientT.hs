{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.ClientT ( ClientT(..)
                      , module Server.ClientT
                      , module Game.UserCmdT
                      , module Game.EdictT
                      , module QCommon.SizeBufT
                      , module QCommon.NetChanT
                      , module Server.ClientFrameT
                      ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Types
import Game.UserCmdT
import Game.EdictT
import QCommon.SizeBufT
import QCommon.NetChanT
import Server.ClientFrameT
import qualified Constants

makeLenses ''ClientT

newClientT :: ClientT
newClientT =
  ClientT { _cState         = 0
          , _cUserInfo      = ""
          , _cLastFrame     = 0
          , _cLastCmd       = newUserCmdT
          , _cCommandMsec   = 0
          , _cFrameLatency  = UV.replicate 16 0 -- LATENCY_COUNTS = 16
          , _cPing          = 0
          , _cMessageSize   = UV.replicate 10 0 -- RATE_MESSAGES = 10
          , _cRate          = 0
          , _cSurpressCount = 0
          , _cEdict         = Nothing
          , _cName          = ""
          , _cMessageLevel  = 0
          , _cDatagram      = newSizeBufT
          , _cDatagramBuf   = ""
          , _cFrames        = V.replicate Constants.updateBackup newClientFrameT
          , _cDownload      = Nothing
          , _cDownloadSize  = 0
          , _cDownloadCount = 0
          , _cLastMessage   = 0
          , _cLastConnect   = 0
          , _cChallenge     = 0
          , _cNetChan       = newNetChanT
          , _cServerIndex   = 0
          }
