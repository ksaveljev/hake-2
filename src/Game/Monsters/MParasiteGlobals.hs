{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MParasiteGlobals where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''MParasiteGlobals

initialMParasiteGlobals :: MParasiteGlobals
initialMParasiteGlobals = MParasiteGlobals
    { _mParasiteSoundPain1   = 0
    , _mParasiteSoundPain2   = 0
    , _mParasiteSoundDie     = 0
    , _mParasiteSoundLaunch  = 0
    , _mParasiteSoundImpact  = 0
    , _mParasiteSoundSuck    = 0
    , _mParasiteSoundReelIn  = 0
    , _mParasiteSoundSight   = 0
    , _mParasiteSoundTap     = 0
    , _mParasiteSoundScratch = 0
    , _mParasiteSoundSearch  = 0
    }
