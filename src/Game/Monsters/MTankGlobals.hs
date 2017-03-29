{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MTankGlobals where

import Control.Lens (makeLenses)

import Types

makeLenses ''MTankGlobals

initialMTankGlobals :: MTankGlobals
initialMTankGlobals =
  MTankGlobals { _mTankSoundThud   = 0
               , _mTankSoundPain   = 0
               , _mTankSoundIdle   = 0
               , _mTankSoundDie    = 0
               , _mTankSoundStep   = 0
               , _mTankSoundSight  = 0
               , _mTankSoundWindUp = 0
               , _mTankSoundStrike = 0
               }
