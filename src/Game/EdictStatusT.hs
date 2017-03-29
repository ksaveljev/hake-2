{-# LANGUAGE TemplateHaskell #-}
module Game.EdictStatusT ( EdictStatusT(..)
                         , module Game.EdictStatusT
                         ) where

import Control.Lens (makeLenses)

import Types

makeLenses ''EdictStatusT

newEdictStatusT :: EdictStatusT
newEdictStatusT =
  EdictStatusT { _eHealth         = 0
               , _eMaxHealth      = 0
               , _eGibHealth      = 0
               , _eDeadFlag       = 0
               , _eShowHostile    = 0
               , _ePowerArmorTime = 0
               , _eViewHeight     = 0
               , _eTakeDamage     = 0
               , _eDmg            = 0
               , _eRadiusDmg      = 0
               , _eDmgRadius      = 0
               }
