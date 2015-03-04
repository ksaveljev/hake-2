{-# LANGUAGE TemplateHaskell #-}
module Game.MoveInfoT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

data MoveInfoT =
  MoveInfoT { _miStartOrigin       :: V3 Float
            , _miStartAngles       :: V3 Float
            , _miEndOrigin         :: V3 Float
            , _miEndAngles         :: V3 Float
            , _miSoundStart        :: Int
            , _miSoundMiddle       :: Int
            , _miSoundEnd          :: Int
            , _miAccel             :: Float
            , _miSpeed             :: Float
            , _miDecel             :: Float
            , _miDistance          :: Float
            , _miWait              :: Float
            , _miState             :: Int
            , _miDir               :: V3 Float
            , _miCurrentSpeed      :: Float
            , _miMoveSpeed         :: Float
            , _miNextSpeed         :: Float
            , _miRemainingDistance :: Float
            , _miDecelDistance     :: Float
            , _miEndFunc           :: IO () -- TODO: ??
            }

makeLenses ''MoveInfoT
