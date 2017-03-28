{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MInfantryGlobals where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MInfantryGlobals

initialMInfantryGlobals :: MInfantryGlobals
initialMInfantryGlobals =
  MInfantryGlobals { _miSoundPain1      = 0
                   , _miSoundPain2      = 0
                   , _miSoundDie1       = 0
                   , _miSoundDie2       = 0
                   , _miSoundGunShot    = 0
                   , _miSoundWeaponCock = 0
                   , _miSoundPunchSwing = 0
                   , _miSoundPunchHit   = 0
                   , _miSoundSight      = 0
                   , _miSoundSearch     = 0
                   , _miSoundIdle       = 0
                   }
