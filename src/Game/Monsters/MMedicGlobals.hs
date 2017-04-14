{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MMedicGlobals where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''MMedicGlobals

initialMMedicGlobals :: MMedicGlobals
initialMMedicGlobals = MMedicGlobals
    { _mMedicSoundIdle1       = 0
    , _mMedicSoundPain1       = 0
    , _mMedicSoundPain2       = 0
    , _mMedicSoundDie         = 0
    , _mMedicSoundSight       = 0
    , _mMedicSoundSearch      = 0
    , _mMedicSoundHookLaunch  = 0
    , _mMedicSoundHookHit     = 0
    , _mMedicSoundHookHeal    = 0
    , _mMedicSoundHookRetract = 0
    }
