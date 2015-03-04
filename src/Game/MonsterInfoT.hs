module Game.MonsterInfoT where

import Linear.V3 (V3)

import Game.MMoveT

data MonsterInfoT =
  MonsterInfoT { miCurrentMove     :: MMoveT
               , miAIFlags         :: Int
               , miNextFrame       :: Int
               , miScale           :: Float
               , miStand           :: IO () -- TODO: ???
               , miIdle            :: IO () -- TODO: ???
               , miSearch          :: IO () -- TODO: ???
               , miWalk            :: IO () -- TODO: ???
               , miRun             :: IO () -- TODO: ???
               , miDodge           :: IO () -- TODO: ???
               , miAttack          :: IO () -- TODO: ???
               , miMelee           :: IO () -- TODO: ???
               , miSight           :: IO () -- TODO: ???
               , miCheckAttack     :: IO () -- TODO: ???
               , miPauseTime       :: Float
               , miAttackFinished  :: Float
               , miSavedGoal       :: V3 Float
               , miSearchTime      :: Float
               , miTrailTime       :: Float
               , miLastSighting    :: V3 Float
               , miAttackState     :: Int
               , miLefty           :: Int
               , miIdleTime        :: Float
               , miLinkCount       :: Int
               , miPowerArmorType  :: Int
               , miPowerArmorPower :: Int
               }
