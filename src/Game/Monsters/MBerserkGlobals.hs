{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MBerserkGlobals where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MBerserkGlobals

initialMBerserkGlobals :: MBerserkGlobals
initialMBerserkGlobals =
  MBerserkGlobals { _mbSoundPain   = 0
                  , _mbSoundDie    = 0
                  , _mbSoundIdle   = 0
                  , _mbSoundPunch  = 0
                  , _mbSoundSight  = 0
                  , _mbSoundSearch = 0
                  }
