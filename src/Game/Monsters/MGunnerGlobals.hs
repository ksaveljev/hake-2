{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MGunnerGlobals where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''MGunnerGlobals

initialMGunnerGlobals :: MGunnerGlobals
initialMGunnerGlobals = MGunnerGlobals
    { _mGunnerSoundPain   = 0
    , _mGunnerSoundPain2  = 0
    , _mGunnerSoundDeath  = 0
    , _mGunnerSoundIdle   = 0
    , _mGunnerSoundOpen   = 0
    , _mGunnerSoundSearch = 0
    , _mGunnerSoundSight  = 0
    }
