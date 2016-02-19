{-# LANGUAGE TemplateHaskell #-}
module QCommon.NetChanT
  ( module QCommon.NetChanT
  ) where

import QCommon.NetAdrT (newNetAdrT)
import QCommon.SizeBufT (newSizeBufT)
import Types

import Control.Lens (makeLenses)

makeLenses ''NetChanT

newNetChanT :: NetChanT
newNetChanT =
  NetChanT { _ncFatalError                   = False
           , _ncSock                         = 0
           , _ncDropped                      = 0
           , _ncLastReceived                 = 0
           , _ncLastSent                     = 0
           , _ncRemoteAddress                = newNetAdrT
           , _ncRemoteQPort                  = 0
           , _ncIncomingSequence             = 0
           , _ncIncomingAcknowledged         = 0
           , _ncIncomingReliableAcknowledged = 0
           , _ncIncomingReliableSequence     = 0
           , _ncOutgoingSequence             = 0
           , _ncReliableSequence             = 0
           , _ncLastReliableSequence         = 0
           , _ncMessage                      = newSizeBufT
           , _ncReliableLength               = 0
           , _ncReliableBuf                  = "" -- size [Constants.maxMsgLen - 16] -- TODO: do we need it ?
           }