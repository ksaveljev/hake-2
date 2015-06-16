{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MGladiatorGlobals where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MGladiatorGlobals

initialMGladiatorGlobals :: MGladiatorGlobals
initialMGladiatorGlobals =
  MGladiatorGlobals { _mGladiatorSoundPain1        = 0
                    , _mGladiatorSoundPain2        = 0
                    , _mGladiatorSoundDie          = 0
                    , _mGladiatorSoundGun          = 0
                    , _mGladiatorSoundCleaverSwing = 0
                    , _mGladiatorSoundCleaverHit   = 0
                    , _mGladiatorSoundCleaverMiss  = 0
                    , _mGladiatorSoundIdle         = 0
                    , _mGladiatorSoundSearch       = 0
                    , _mGladiatorSoundSight        = 0
                    }
