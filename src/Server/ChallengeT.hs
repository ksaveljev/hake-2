{-# LANGUAGE TemplateHaskell #-}
module Server.ChallengeT
  ( module Server.ChallengeT
  ) where

import QCommon.NetAdrT (newNetAdrT)
import Types

import Control.Lens (makeLenses)

makeLenses ''ChallengeT

newChallengeT :: ChallengeT
newChallengeT =
  ChallengeT { _chAdr       = newNetAdrT
             , _chChallenge = 0
             , _chTime      = 0
             }