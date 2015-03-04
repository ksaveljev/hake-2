module Game.UserCmdT where

import Data.Int (Int8, Int16)
import Linear.V3 (V3)

data UserCmdT =
  UserCmdT { ucMsec        :: Int8
           , ucButtons     :: Int8
           , ucAngles      :: V3 Int16
           , ucForwardMove :: Int16
           , ucSideMove    :: Int16
           , ucUpMove      :: Int16
           , ucImpulse     :: Int8
           , ucLightLevel  :: Int8
           }
