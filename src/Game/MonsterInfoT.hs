{-# LANGUAGE TemplateHaskell #-}
module Game.MonsterInfoT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

import Game.MMoveT

data MonsterInfoT =
  MonsterInfoT { _miCurrentMove     :: MMoveT
               , _miAIFlags         :: Int
               , _miNextFrame       :: Int
               , _miScale           :: Float
               , _miStand           :: IO () -- TODO: ???
               , _miIdle            :: IO () -- TODO: ???
               , _miSearch          :: IO () -- TODO: ???
               , _miWalk            :: IO () -- TODO: ???
               , _miRun             :: IO () -- TODO: ???
               , _miDodge           :: IO () -- TODO: ???
               , _miAttack          :: IO () -- TODO: ???
               , _miMelee           :: IO () -- TODO: ???
               , _miSight           :: IO () -- TODO: ???
               , _miCheckAttack     :: IO () -- TODO: ???
               , _miPauseTime       :: Float
               , _miAttackFinished  :: Float
               , _miSavedGoal       :: V3 Float
               , _miSearchTime      :: Float
               , _miTrailTime       :: Float
               , _miLastSighting    :: V3 Float
               , _miAttackState     :: Int
               , _miLefty           :: Int
               , _miIdleTime        :: Float
               , _miLinkCount       :: Int
               , _miPowerArmorType  :: Int
               , _miPowerArmorPower :: Int
               }

makeLenses ''MonsterInfoT
