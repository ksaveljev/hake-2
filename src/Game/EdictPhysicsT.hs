{-# LANGUAGE TemplateHaskell #-}
module Game.EdictPhysicsT ( EdictPhysicsT(..)
                          , module Game.EdictPhysicsT
                          ) where

import Control.Lens (makeLenses)
import Linear (V3(..))

import Types

makeLenses ''EdictPhysicsT

newEdictPhysicsT :: EdictPhysicsT
newEdictPhysicsT =
  EdictPhysicsT { _eAngle       = 0
                , _eSpeed       = 0
                , _eAccel       = 0
                , _eDecel       = 0
                , _eMoveDir     = V3 0 0 0
                , _ePos1        = V3 0 0 0
                , _ePos2        = V3 0 0 0
                , _eVelocity    = V3 0 0 0
                , _eAVelocity   = V3 0 0 0
                , _eMass        = 0
                , _eAirFinished = 0
                , _eGravity     = 0
                , _eYawSpeed    = 0
                , _eIdealYaw    = 0
                }
