{-# LANGUAGE TemplateHaskell #-}
module Server.ServerStaticT where

import Data.Word (Word8)
import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.EntityStateT
import Server.ClientT
import Server.ChallengeT
import QCommon.SizeBufT

data ServerStaticT =
  ServerStaticT { _ssInitialized        :: Bool
                , _ssRealTime           :: Int
                , _ssMapCmd             :: B.ByteString
                , _ssSpawnCount         :: Int
                , _ssClients            :: UV.Vector ClientT
                , _ssNumClientEntities  :: Int
                , _ssNextClientEntities :: Int
                , _ssClientEntities     :: UV.Vector EntityStateT
                , _ssLastHeartbeat      :: Int
                , _ssChallenges         :: UV.Vector ChallengeT
                , _ssDemoFile           :: B.ByteString
                , _ssDemoMulticast      :: SizeBufT
                , _ssDemoMulticastBuf   :: UV.Vector Word8
                }

makeLenses ''ServerStaticT
