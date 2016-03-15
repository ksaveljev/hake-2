{-# LANGUAGE TemplateHaskell #-}
module Server.ServerStaticT
  ( module Server.ServerStaticT
  ) where

import qualified Constants
import           QCommon.SizeBufT (newSizeBufT)
import           Server.ChallengeT (newChallengeT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.Vector as V

makeLenses ''ServerStaticT

newServerStaticT :: ServerStaticT
newServerStaticT =
  ServerStaticT { _ssInitialized        = False
                , _ssRealTime           = 0
                , _ssMapCmd             = B.empty
                , _ssSpawnCount         = 0
                , _ssClients            = V.empty
                , _ssNumClientEntities  = 0
                , _ssNextClientEntities = 0
                , _ssClientEntities     = V.empty
                , _ssLastHeartbeat      = 0
                , _ssChallenges         = V.replicate Constants.maxChallenges newChallengeT
                , _ssDemoFile           = Nothing
                , _ssDemoMulticast      = newSizeBufT
                , _ssDemoMulticastBuf   = B.empty
                }