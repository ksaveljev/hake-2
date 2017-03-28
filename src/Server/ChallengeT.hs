{-# LANGUAGE TemplateHaskell #-}
module Server.ChallengeT where

import Control.Lens (makeLenses)

import QCommon.NetAdrT

data ChallengeT =
  ChallengeT { _chAdr       :: NetAdrT
             , _chChallenge :: Int
             , _chTime      :: Int
             }

makeLenses ''ChallengeT

newChallengeT :: ChallengeT
newChallengeT =
  ChallengeT { _chAdr       = newNetAdrT
             , _chChallenge = 0
             , _chTime      = 0
             }
