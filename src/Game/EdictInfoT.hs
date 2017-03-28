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
  EdictInfoT { _eiModel        = Nothing
             , _eiMessage      = Nothing
             , _eiTarget       = Nothing
             , _eiTargetName   = Nothing
             , _eiKillTarget   = Nothing
             , _eiTeam         = Nothing
             , _eiPathTarget   = Nothing
             , _eiDeathTarget  = Nothing
             , _eiCombatTarget = Nothing
             , _eiMap          = Nothing
             }
