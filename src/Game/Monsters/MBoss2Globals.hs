{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MBoss2Globals where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''MBoss2Globals

initialMBoss2Globals :: MBoss2Globals
initialMBoss2Globals = MBoss2Globals
    { _mb2SoundPain1   = 0
    , _mb2SoundPain2   = 0
    , _mb2SoundPain3   = 0
    , _mb2SoundDeath   = 0
    , _mb2SoundSearch1 = 0
    }