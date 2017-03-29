{-# LANGUAGE TemplateHaskell #-}
module Game.UserCmdT where

import           Control.Lens (makeLenses)
import           Data.Int     (Int8, Int16)
import           Linear.V3    (V3(..))

import           Types

makeLenses ''UserCmdT

newUserCmdT :: UserCmdT
newUserCmdT = UserCmdT
    { _ucMsec        = 0
    , _ucButtons     = 0
    , _ucAngles      = V3 0 0 0
    , _ucForwardMove = 0
    , _ucSideMove    = 0
    , _ucUpMove      = 0
    , _ucImpulse     = 0
    , _ucLightLevel  = 0
    }