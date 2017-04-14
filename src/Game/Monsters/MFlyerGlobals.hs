{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MFlyerGlobals where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''MFlyerGlobals

initialMFlyerGlobals :: MFlyerGlobals
initialMFlyerGlobals = MFlyerGlobals
    { _mFlyerNextMove     = 0
    , _mFlyerSoundSight   = 0
    , _mFlyerSoundIdle    = 0
    , _mFlyerSoundPain1   = 0
    , _mFlyerSoundPain2   = 0
    , _mFlyerSoundSlash   = 0
    , _mFlyerSoundSproing = 0
    , _mFlyerSoundDie     = 0
    }
