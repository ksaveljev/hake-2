{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MBoss31Globals where

import Control.Lens (makeLenses)

import Types

makeLenses ''MBoss31Globals

initialMBoss31Globals :: MBoss31Globals
initialMBoss31Globals =
  MBoss31Globals { _mb31SoundPain1     = 0
                 , _mb31SoundPain2     = 0
                 , _mb31SoundPain3     = 0
                 , _mb31SoundIdle      = 0
                 , _mb31SoundDeath     = 0
                 , _mb31SoundSearch1   = 0
                 , _mb31SoundSearch2   = 0
                 , _mb31SoundSearch3   = 0
                 , _mb31SoundAttack1   = 0
                 , _mb31SoundAttack2   = 0
                 , _mb31SoundFireGun   = 0
                 , _mb31SoundStepLeft  = 0
                 , _mb31SoundStepRight = 0
                 , _mb31SoundDeathHit  = 0
                 }
