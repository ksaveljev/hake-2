{-# LANGUAGE TemplateHaskell #-}
module Game.MonsterInfoT ( MonsterInfoT(..)
                         , module Game.MonsterInfoT
                         , module Game.MMoveT
                         ) where

import Linear.V3 (V3(..))
import Control.Lens (makeLenses)

import Internal
import Game.MMoveT

makeLenses ''MonsterInfoT

newMonsterInfoT :: MonsterInfoT
newMonsterInfoT =
  MonsterInfoT { _miCurrentMove     = Nothing
               , _miAIFlags         = 0
               , _miNextFrame       = 0
               , _miScale           = 0
               , _miStand           = Nothing
               , _miIdle            = Nothing
               , _miSearch          = Nothing
               , _miWalk            = Nothing
               , _miRun             = Nothing
               , _miDodge           = Nothing
               , _miAttack          = Nothing
               , _miMelee           = Nothing
               , _miSight           = Nothing
               , _miCheckAttack     = Nothing
               , _miPauseTime       = 0
               , _miAttackFinished  = 0
               , _miSavedGoal       = V3 0 0 0
               , _miSearchTime      = 0
               , _miTrailTime       = 0
               , _miLastSighting    = V3 0 0 0
               , _miAttackState     = 0
               , _miLefty           = 0
               , _miIdleTime        = 0
               , _miLinkCount       = 0
               , _miPowerArmorType  = 0
               , _miPowerArmorPower = 0
               }
