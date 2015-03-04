module Game.MoveInfoT where

import Linear.V3 (V3)

data MoveInfoT =
  MoveInfoT { miStartOrigin       :: V3 Float
            , miStartAngles       :: V3 Float
            , miEndOrigin         :: V3 Float
            , miEndAngles         :: V3 Float
            , miSoundStart        :: Int
            , miSoundMiddle       :: Int
            , miSoundEnd          :: Int
            , miAccel             :: Float
            , miSpeed             :: Float
            , miDecel             :: Float
            , miDistance          :: Float
            , miWait              :: Float
            , miState             :: Int
            , miDir               :: V3 Float
            , miCurrentSpeed      :: Float
            , miMoveSpeed         :: Float
            , miNextSpeed         :: Float
            , miRemainingDistance :: Float
            , miDecelDistance     :: Float
            , miEndFunc           :: IO () -- TODO: ??
            }
