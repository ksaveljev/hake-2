{-# LANGUAGE TemplateHaskell #-}
module Client.ClientStaticT where

import Control.Lens (makeLenses)
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
                , _csDownload           :: B.ByteString
                , _csDownloadTempName   :: B.ByteString
                , _csDownloadName       :: B.ByteString
                , _csDownloadNumber     :: Int
                , _csDownloadType       :: Int
                , _csDownloadPercent    :: Int
                , _csDemoRecording      :: Bool
                , _csDemoWaiting        :: Bool
                , _csDemoFile           :: B.ByteString
                }

makeLenses ''ClientStaticT
