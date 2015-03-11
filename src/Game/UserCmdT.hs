{-# LANGUAGE TemplateHaskell #-}
module Game.UserCmdT where

import Data.Int (Int8, Int16)
import Linear.V3 (V3)
import Control.Lens (makeLenses)

data UserCmdT =
  UserCmdT { _ucMsec        :: Int8
           , _ucButtons     :: Int8
           , _ucAngles      :: V3 Int16
           , _ucForwardMove :: Int16
           , _ucSideMove    :: Int16
           , _ucUpMove      :: Int16
           , _ucImpulse     :: Int8
           , _ucLightLevel  :: Int8
           }

makeLenses ''UserCmdT

newUserCmdT :: UserCmdT
newUserCmdT = undefined -- TODO
