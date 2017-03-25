{-# LANGUAGE TemplateHaskell #-}
module Server.ChallengeT
    ( module Server.ChallengeT
    ) where

import           Control.Lens    (makeLenses)

import           QCommon.NetAdrT (newNetAdrT)
import           Types

makeLenses ''ChallengeT

newChallengeT :: ChallengeT
newChallengeT = ChallengeT
    { _chAdr       = newNetAdrT
    , _chChallenge = 0
    , _chTime      = 0
    }