{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MChickGlobals where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''MChickGlobals

initialMChickGlobals :: MChickGlobals
initialMChickGlobals = MChickGlobals
    { _mChickSoundMissilePrelaunch = 0
    , _mChickSoundMissileLaunch    = 0
    , _mChickSoundMeleeSwing       = 0
    , _mChickSoundMeleeHit         = 0
    , _mChickSoundMissileReload    = 0
    , _mChickSoundDeath1           = 0
    , _mChickSoundDeath2           = 0
    , _mChickSoundFallDown         = 0
    , _mChickSoundIdle1            = 0
    , _mChickSoundIdle2            = 0
    , _mChickSoundPain1            = 0
    , _mChickSoundPain2            = 0
    , _mChickSoundPain3            = 0
    , _mChickSoundSight            = 0
    , _mChickSoundSearch           = 0
    }