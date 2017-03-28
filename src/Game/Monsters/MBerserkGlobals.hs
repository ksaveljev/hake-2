{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MBerserkGlobals where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MBerserkGlobals

initialMBerserkGlobals :: MBerserkGlobals
initialMBerserkGlobals =
  MBerserkGlobals { _mBerserkSoundPain   = 0
                  , _mBerserkSoundDie    = 0
                  , _mBerserkSoundIdle   = 0
                  , _mBerserkSoundPunch  = 0
                  , _mBerserkSoundSight  = 0
                  , _mBerserkSoundSearch = 0
                  }
