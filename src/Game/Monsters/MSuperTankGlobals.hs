{-# LANGUAGE TemplateHaskell #-}
module Game.Monsters.MSuperTankGlobals where

import Control.Lens (makeLenses)

import Types

makeLenses ''MSuperTankGlobals

initialMSuperTankGlobals :: MSuperTankGlobals
initialMSuperTankGlobals =
  MSuperTankGlobals { _mSuperTankSoundPain1   = 0
                    , _mSuperTankSoundPain2   = 0
                    , _mSuperTankSoundPain3   = 0
                    , _mSuperTankSoundDeath   = 0
                    , _mSuperTankSoundSearch1 = 0
                    , _mSuperTankSoundSearch2 = 0
                    , _mSuperTankTreadSound   = 0
                    }
