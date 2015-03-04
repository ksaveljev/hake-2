module Game.MoveInfo where

import Linear.V3 (V3)

data MoveInfo = MoveInfo { moveInfoStartOrigin       :: V3 Float
                         , moveInfoStartAngles       :: V3 Float
                         , moveInfoEndOrigin         :: V3 Float
                         , moveInfoEndAngles         :: V3 Float
                         , moveInfoSoundStart        :: Int
                         , moveInfoSoundMiddle       :: Int
                         , moveInfoSoundEnd          :: Int
                         , moveInfoAccel             :: Float
                         , moveInfoSpeed             :: Float
                         , moveInfoDecel             :: Float
                         , moveInfoDistance          :: Float
                         , moveInfoWait              :: Float
                         , moveInfoState             :: Int
                         , moveInfoDir               :: V3 Float
                         , moveInfoCurrentSpeed      :: Float
                         , moveInfoMoveSpeed         :: Float
                         , moveInfoNextSpeed         :: Float
                         , moveInfoRemainingDistance :: Float
                         , moveInfoDecelDistance     :: Float
                         , moveInfoEndFunc           :: IO () -- TODO: ??
                         }
