{-# LANGUAGE TemplateHaskell #-}
module Client.ClientStaticT
  ( module Client.ClientStaticT
  ) where

import           QCommon.NetChanT (newNetChanT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B

makeLenses ''ClientStaticT

newClientStaticT :: ClientStaticT
newClientStaticT =
  ClientStaticT { _csState              = 0
                , _csKeyDest            = 0
                , _csFrameCount         = 0
                , _csRealTime           = 0
                , _csFrameTime          = 0
                , _csDisableScreen      = 0
                , _csDisableServerCount = 0
                , _csServerName         = B.empty
                , _csConnectTime        = 0
                , _csQuakePort          = 0
                , _csNetChan            = newNetChanT
                , _csServerProtocol     = 0
                , _csChallenge          = 0
                , _csDownload           = Nothing
                , _csDownloadTempName   = B.empty
                , _csDownloadName       = B.empty
                , _csDownloadNumber     = 0
                , _csDownloadType       = 0
                , _csDownloadPercent    = 0
                , _csDemoRecording      = False
                , _csDemoWaiting        = False
                , _csDemoFile           = Nothing
                }