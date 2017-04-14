{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MHoverGlobals where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''MHoverGlobals

initialMHoverGlobals :: MHoverGlobals
initialMHoverGlobals = MHoverGlobals
    { _mHoverSoundPain1   = 0
    , _mHoverSoundPain2   = 0
    , _mHoverSoundDeath1  = 0
    , _mHoverSoundDeath2  = 0
    , _mHoverSoundSight   = 0
    , _mHoverSoundSearch1 = 0
    , _mHoverSoundSearch2 = 0
    }
