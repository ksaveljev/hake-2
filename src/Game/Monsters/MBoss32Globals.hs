{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MBoss32Globals where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MBoss32Globals

initialMBoss32Globals :: MBoss32Globals
initialMBoss32Globals =
  MBoss32Globals { _mb32SoundPain4        = 0
                 , _mb32SoundPain5        = 0
                 , _mb32SoundPain6        = 0
                 , _mb32SoundDeath        = 0
                 , _mb32SoundStepLeft     = 0
                 , _mb32SoundStepRight    = 0
                 , _mb32SoundAttackBfg    = 0
                 , _mb32SoundBrainSplorch = 0
                 , _mb32SoundPreRailGun   = 0
                 , _mb32SoundPopUp        = 0
                 , _mb32SoundTaunt1       = 0
                 , _mb32SoundTaunt2       = 0
                 , _mb32SoundTaunt3       = 0
                 , _mb32SoundHit          = 0
                 }
