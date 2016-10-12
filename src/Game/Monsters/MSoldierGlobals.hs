{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MSoldierGlobals where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''MSoldierGlobals

initialMSoldierGlobals :: MSoldierGlobals
initialMSoldierGlobals = MSoldierGlobals
    { _msSoundIdle       = 0
    , _msSoundSight1     = 0
    , _msSoundSight2     = 0
    , _msSoundPainLight  = 0
    , _msSoundPain       = 0
    , _msSoundPainSS     = 0
    , _msSoundDeathLight = 0
    , _msSoundDeath      = 0
    , _msSoundDeathSS    = 0
    , _msSoundCock       = 0
    }