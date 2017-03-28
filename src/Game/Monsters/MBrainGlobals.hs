{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MBrainGlobals where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MBrainGlobals

initialMBrainGlobals :: MBrainGlobals
initialMBrainGlobals =
  MBrainGlobals { _mBrainSoundChestOpen        = 0
                , _mBrainSoundTentaclesExtend  = 0
                , _mBrainSoundTentaclesRetract = 0
                , _mBrainSoundDeath            = 0
                , _mBrainSoundIdle1            = 0
                , _mBrainSoundIdle2            = 0
                , _mBrainSoundIdle3            = 0
                , _mBrainSoundPain1            = 0
                , _mBrainSoundPain2            = 0
                , _mBrainSoundSight            = 0
                , _mBrainSoundSearch           = 0
                , _mBrainSoundMelee1           = 0
                , _mBrainSoundMelee2           = 0
                , _mBrainSoundMelee3           = 0
                }
