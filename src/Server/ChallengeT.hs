{-# LANGUAGE TemplateHaskell #-}
module Server.ChallengeT where

import Control.Lens (makeLenses)

import QCommon.NetAdrT

data ChallengeT =
  ChallengeT { _cAdr       :: NetAdrT
             , _cChallenge :: Int
             , _cTime      :: Int
             }

makeLenses ''ChallengeT
