{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.ClientStaticT where

import Control.Lens (makeLenses)
import System.IO (Handle)
import qualified Data.ByteString as B

import QCommon.NetChanT

data ClientStaticT =
  ClientStaticT { _csState              :: Int
                , _csKeyDest            :: Int
                , _csFrameCount         :: Int
                , _csRealTime           :: Int
                , _csFrameTime          :: Float
                , _csDisableScreen      :: Float
                , _csDisableServerCount :: Int
                , _csServerName         :: B.ByteString
                , _csConnectTime        :: Float
                , _csQuakePort          :: Int
                , _csNetChan            :: NetChanT
                , _csServerProtocol     :: Int
                , _csChallenge          :: Int
                , _csDownload           :: Maybe Handle
                , _csDownloadTempName   :: B.ByteString
                , _csDownloadName       :: B.ByteString
                , _csDownloadNumber     :: Int
                , _csDownloadType       :: Int
                , _csDownloadPercent    :: Int
                , _csDemoRecording      :: Bool
                , _csDemoWaiting        :: Bool
                , _csDemoFile           :: Maybe Handle
                }

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
                , _csServerName         = ""
                , _csConnectTime        = 0
                , _csQuakePort          = 0
                , _csNetChan            = newNetChanT
                , _csServerProtocol     = 0
                , _csChallenge          = 0
                , _csDownload           = Nothing
                , _csDownloadTempName   = ""
                , _csDownloadName       = ""
                , _csDownloadNumber     = 0
                , _csDownloadType       = 0
                , _csDownloadPercent    = 0
                , _csDemoRecording      = False
                , _csDemoWaiting        = False
                , _csDemoFile           = Nothing
                }
