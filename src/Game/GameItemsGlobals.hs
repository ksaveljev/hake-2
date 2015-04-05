{-# LANGUAGE TemplateHaskell #-}
module Game.GameItemsGlobals where

import Control.Lens (makeLenses)

import Internal
import Game.GItemArmorT
import qualified Constants

makeLenses ''GameItemsGlobals

initialGameItemsGlobals :: GameItemsGlobals
initialGameItemsGlobals =
  GameItemsGlobals { _giJacketArmorInfo      = GItemArmorT 25 50 0.30 0 Constants.armorJacket
                   , _giCombatArmorInfo      = GItemArmorT 50 100 0.60 0.30 Constants.armorCombat
                   , _giBodyArmorInfo        = GItemArmorT 100 200 0.80 0.60 Constants.armorBody
                   , _giQuakeDropTimeoutHack = 0
                   , _giJacketArmorIndex     = GItemReference 0
                   , _giCombatArmorIndex     = GItemReference 0
                   , _giBodyArmorIndex       = GItemReference 0
                   , _giPowerScreenIndex     = GItemReference 0
                   , _giPowerShieldIndex     = GItemReference 0
                   }
