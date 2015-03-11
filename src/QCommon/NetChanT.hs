{-# LANGUAGE TemplateHaskell #-}
module QCommon.NetChanT where

import Data.Word (Word8)
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import QCommon.NetAdrT
import QCommon.SizeBufT

data NetChanT =
  NetChanT { _ncFatalError                   :: Bool
           , _ncSock                         :: Int
           , _ncDropped                      :: Int
           , _ncLastReceived                 :: Int
           , _ncLastSent                     :: Int
           , _ncRemoteAddress                :: NetAdrT
           , _ncQPort                        :: Int
           , _ncIncomingSequence             :: Int
           , _ncIncomingAcknowledged         :: Int
           , _ncIncomingReliableAcknowledged :: Int
           , _ncIncomingReliableSequence     :: Int
           , _ncOutgoingSequence             :: Int
           , _ncLastReliableSequence         :: Int
           , _ncMessage                      :: SizeBufT
           , _ncMessageBuf                   :: UV.Vector Word8
           , _ncReliableLength               :: Int
           , _ncReliableBuf                  :: UV.Vector Word8
           }

makeLenses ''NetChanT
