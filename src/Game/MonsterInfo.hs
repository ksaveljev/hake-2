module Game.MonsterInfo where

import Linear.V3 (V3)

import Game.MMove

data MonsterInfo = MonsterInfo { monsterInfoCurrentMove     :: MMove
                               , monsterInfoAIFlags         :: Int
                               , monsterInfoNextFrame       :: Int
                               , monsterInfoScale           :: Float
                               , monsterInfoStand           :: IO () -- TODO: ???
                               , monsterInfoIdle            :: IO () -- TODO: ???
                               , monsterInfoSearch          :: IO () -- TODO: ???
                               , monsterInfoWalk            :: IO () -- TODO: ???
                               , monsterInfoRun             :: IO () -- TODO: ???
                               , monsterInfoDodge           :: IO () -- TODO: ???
                               , monsterInfoAttack          :: IO () -- TODO: ???
                               , monsterInfoMelee           :: IO () -- TODO: ???
                               , monsterInfoSight           :: IO () -- TODO: ???
                               , monsterInfoCheckAttack     :: IO () -- TODO: ???
                               , monsterInfoPauseTime       :: Float
                               , monsterInfoAttackFinished  :: Float
                               , monsterInfoSavedGoal       :: V3 Float
                               , monsterInfoSearchTime      :: Float
                               , monsterInfoTrailTime       :: Float
                               , monsterInfoLastSighting    :: V3 Float
                               , monsterInfoAttackState     :: Int
                               , monsterInfoLefty           :: Int
                               , monsterInfoIdleTime        :: Float
                               , monsterInfoLinkCount       :: Int
                               , monsterInfoPowerArmorType  :: Int
                               , monsterInfoPowerArmorPower :: Int
                               }
