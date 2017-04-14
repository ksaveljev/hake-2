{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MFloatGlobals where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''MFloatGlobals

initialMFloatGlobals :: MFloatGlobals
initialMFloatGlobals = MFloatGlobals
    { _mFloatSoundAttack2 = 0
    , _mFloatSoundAttack3 = 0
    , _mFloatSoundDeath1  = 0
    , _mFloatSoundIdle    = 0
    , _mFloatSoundPain1   = 0
    , _mFloatSoundPain2   = 0
    , _mFloatSoundSight   = 0
    }
