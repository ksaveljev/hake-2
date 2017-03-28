{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MMutantGlobals where

import Control.Lens (makeLenses)

import Internal

makeLenses ''MMutantGlobals

initialMMutantGlobals :: MMutantGlobals
initialMMutantGlobals =
  MMutantGlobals { _mMutantSoundSwing  = 0
                 , _mMutantSoundHit    = 0
                 , _mMutantSoundHit2   = 0
                 , _mMutantSoundDeath  = 0
                 , _mMutantSoundIdle   = 0
                 , _mMutantSoundPain1  = 0
                 , _mMutantSoundPain2  = 0
                 , _mMutantSoundSight  = 0
                 , _mMutantSoundSearch = 0
                 , _mMutantSoundStep1  = 0
                 , _mMutantSoundStep2  = 0
                 , _mMutantSoundStep3  = 0
                 , _mMutantSoundThud   = 0
                 }
