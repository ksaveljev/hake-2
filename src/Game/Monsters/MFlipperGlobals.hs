{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MFlipperGlobals where

import Control.Lens (makeLenses)

import Types

makeLenses ''MFlipperGlobals

initialMFlipperGlobals :: MFlipperGlobals
initialMFlipperGlobals =
  MFlipperGlobals { _mFlipperSoundChomp  = 0
                  , _mFlipperSoundAttack = 0
                  , _mFlipperSoundPain1  = 0
                  , _mFlipperSoundPain2  = 0
                  , _mFlipperSoundDeath  = 0
                  , _mFlipperSoundIdle   = 0
                  , _mFlipperSoundSearch = 0
                  , _mFlipperSoundSight  = 0
                  }
