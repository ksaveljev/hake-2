module QCommon.NetChanT where

import Data.Int (Int8)
import qualified Data.Vector.Unboxed as UV

import QCommon.NetAdrT
import QCommon.SizeBufT

data NetChanT =
  NetChanT { ncFatalError                   :: Bool
           , ncSock                         :: Int
           , ncDropped                      :: Int
           , ncLastReceived                 :: Int
           , ncLastSent                     :: Int
           , ncRemoteAddress                :: NetAdrT
           , ncQPort                        :: Int
           , ncIncomingSequence             :: Int
           , ncIncomingAcknowledged         :: Int
           , ncIncomingReliableAcknowledged :: Int
           , ncIncomingReliableSequence     :: Int
           , ncOutgoingSequence             :: Int
           , ncLastReliableSequence         :: Int
           , ncMessage                      :: SizeBufT
           , ncMessageBuf                   :: UV.Vector Int8 -- TODO: Word8?
           , ncReliableLength               :: Int
           , ncReliableBuf                  :: UV.Vector Int8 -- TODO: Word8?
           }
