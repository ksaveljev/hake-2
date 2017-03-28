{-# LANGUAGE TemplateHaskell #-}
module Game.MoveInfoT where

import Linear.V3 (V3(..))
import Control.Lens (makeLenses)

import Internal

makeLenses ''MoveInfoT

newMoveInfoT :: MoveInfoT
newMoveInfoT =
  MoveInfoT { _miStartOrigin       = V3 0 0 0
            , _miStartAngles       = V3 0 0 0
            , _miEndOrigin         = V3 0 0 0
            , _miEndAngles         = V3 0 0 0
            , _miSoundStart        = 0
            , _miSoundMiddle       = 0
            , _miSoundEnd          = 0
            , _miAccel             = 0
            , _miSpeed             = 0
            , _miDecel             = 0
            , _miDistance          = 0
            , _miWait              = 0
            , _miState             = 0
            , _miDir               = V3 0 0 0
            , _miCurrentSpeed      = 0
            , _miMoveSpeed         = 0
            , _miNextSpeed         = 0
            , _miRemainingDistance = 0
            , _miDecelDistance     = 0
            , _miEndFunc           = Nothing
            }
