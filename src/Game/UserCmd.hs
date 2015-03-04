module Game.UserCmd where

import Data.Int (Int8, Int16)
import Linear.V3 (V3)

data UserCmd =
  UserCmd { userCmdMsec        :: Int8
          , userCmdButtons     :: Int8
          , userCmdAngles      :: V3 Int16
          , userCmdForwardMove :: Int16
          , userCmdSideMove    :: Int16
          , userCmdUpMove      :: Int16
          , userCmdImpulse     :: Int8
          , userCmdLightLevel  :: Int8
          }
