module Server.Types where

import Data.Int (Int8)
import Linear.V3 (V3)
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as B

import QCommon.Types
import Game.Types

data AreaNode = AreaNode { areaNodeAxis          :: Int
                         , areaNodeDist          :: Float
                         , areaNodeChildren      :: (AreaNode, AreaNode)
                         , areaNodeTriggerEdicts :: Link
                         , areaNodeSolidEdicts   :: Link
                         }

data Challenge = Challenge { challengeAdr       :: NetAdr
                           , challengeChallenge :: Int
                           , challengeTime      :: Int
                           }

data ClientFrame = ClientFrame { clientFrameAreaBytes   :: Int
                               , clientFrameAreaBits    :: UV.Vector Int8 -- TODO: Word8?
                               , clientFramePlayerState :: PlayerState
                               , clientFrameNumEntities :: Int
                               , clientFrameFirstEntity :: Int
                               , clientFrameSentTime    :: Int
                               }

data Client = Client { clientState         :: Int
                     , clientUserInfo      :: B.ByteString
                     , clientLastFrame     :: Int
                     , clientLastCmd       :: UserCmd
                     , clientCommandMsec   :: Int
                     , clientFrameLatency  :: UV.Vector Int
                     , clientPing          :: Int
                     , clientMessageSize   :: UV.Vector Int
                     , clientRate          :: Int
                     , clientSurpressCount :: Int
                     , clientEdict         :: Edict
                     , clientName          :: B.ByteString
                     , clientMessageLevel  :: Int
                     , clientDatagram      :: SizeBuf
                     , clientDatagramBuf   :: UV.Vector Int8 -- TODO: Word8 ?
                     , clientFrames        :: UV.Vector ClientFrame
                     , clientDownload      :: UV.Vector Int8 -- TODO: Word8 ?
                     , clientDownloadSize  :: Int
                     , clientDownloadCount :: Int
                     , clientLastMessage   :: Int
                     , clientLastConnect   :: Int
                     , clientChallenge     :: Int
                     , clientNetChan       :: NetChan
                     , clientServerIndex   :: Int
                     }

data MoveClip = MoveClip { moveClipBoxMins     :: V3 Float
                         , moveClipBoxMaxs     :: V3 Float
                         , moveClipMins        :: V3 Float -- TODO: are we sure it is V3 ?
                         , moveClipMaxs        :: V3 Float -- TODO: are we sure it is V3 ?
                         , moveClipMins2       :: V3 Float
                         , moveClipMaxs2       :: V3 Float
                         , moveClipStart       :: V3 Float -- TODO: are we sure it is V3 ?
                         , moveClipEnd         :: V3 Float -- TODO: are we sure it is V3 ?
                         , moveClipTrace       :: Trace
                         , moveClipPassEdict   :: Edict
                         , moveClipContentMask :: Int
                         }

data ServerStatic = ServerStatic { serverStaticInitialized        :: Bool
                                 , serverStaticRealTime           :: Int
                                 , serverStaticMapCmd             :: B.ByteString
                                 , serverStaticSpawnCount         :: Int
                                 , serverStaticClients            :: UV.Vector Client
                                 , serverStaticNumClientEntities  :: Int
                                 , serverStaticNextClientEntities :: Int
                                 , serverStaticClientEntities     :: UV.Vector EntityState
                                 , serverStaticLastHeartbeat      :: Int
                                 , serverStaticChallenges         :: UV.Vector Challenge
                                 , serverStaticDemoFile           :: FilePath -- TODO: ???????
                                 , serverStaticDemoMulticast      :: SizeBuf
                                 , serverStaticDemoMulticastBuf   :: UV.Vector Int8 -- TODO: Word8 ?
                                 }

data Server = Server { serverState         :: Int
                     , serverAttractLoop   :: Bool
                     , serverLoadGame      :: Bool
                     , serverTime          :: Int
                     , serverFrameNum      :: Int
                     , serverName          :: B.ByteString
                     , serverModels        :: UV.Vector CModel
                     , serverConfigStrings :: UV.Vector B.ByteString
                     , serverBaselines     :: UV.Vector EntityState
                     , serverMulticast     :: SizeBuf
                     , serverMulticastBuf  :: UV.Vector Int8 -- TODO: Word8 ?
                     , serverDemoFile      :: FilePath -- TODO: ???????
                     , serverTimeDemo      :: Int
                     }
