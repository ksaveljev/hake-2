{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.EdictInfoT ( EdictInfoT(..)
                       , module Game.EdictInfoT
                       ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''EdictInfoT

newEdictInfoT :: EdictInfoT
newEdictInfoT =
  EdictInfoT { _esModel        = Nothing
             , _esMessage      = Nothing
             , _esClassName    = ""
             , _esTarget       = Nothing
             , _esTargetName   = Nothing
             , _esKillTarget   = Nothing
             , _esTeam         = Nothing
             , _esPathTarget   = Nothing
             , _esDeathTarget  = Nothing
             , _esCombatTarget = Nothing
             , _esMap          = Nothing
             }
