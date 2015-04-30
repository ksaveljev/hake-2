{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Server.ServerStaticT ( ServerStaticT(..)
                            , module Server.ServerStaticT
                            , module Server.ChallengeT
                            , module Server.ClientT
                            , module Game.EntityStateT
                            ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
import Game.EntityStateT
import Server.ChallengeT
import Server.ClientT
import qualified Constants

makeLenses ''ServerStaticT

newServerStaticT :: ServerStaticT
newServerStaticT =
  ServerStaticT { _ssInitialized        = False
                , _ssRealTime           = 0
                , _ssMapCmd             = ""
                , _ssSpawnCount         = 0
                , _ssClients            = V.empty
                , _ssNumClientEntities  = 0
                , _ssNextClientEntities = 0
                , _ssClientEntities     = V.empty
                , _ssLastHeartbeat      = 0
                , _ssChallenges         = V.replicate Constants.maxChallenges newChallengeT
                , _ssDemoFile           = Nothing
                , _ssDemoMulticast      = newSizeBufT
                , _ssDemoMulticastBuf   = ""
                }
