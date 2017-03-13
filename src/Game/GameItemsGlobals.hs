{-# LANGUAGE TemplateHaskell #-}
module Game.GameItemsGlobals
  ( module Game.GameItemsGlobals
  ) where

import qualified Constants
import           Types

import           Control.Lens (makeLenses)

makeLenses ''GameItemsGlobals

initialGameItemsGlobals :: GameItemsGlobals
initialGameItemsGlobals =
  GameItemsGlobals { _giJacketArmorInfo      = GItemArmorT 25 50 0.30 0 Constants.armorJacket
                   , _giCombatArmorInfo      = GItemArmorT 50 100 0.60 0.30 Constants.armorCombat
                   , _giBodyArmorInfo        = GItemArmorT 100 200 0.80 0.60 Constants.armorBody
                   , _giQuakeDropTimeoutHack = 0
                   , _giJacketArmorIndex     = Ref Constants.noParent 0
                   , _giCombatArmorIndex     = Ref Constants.noParent 0
                   , _giBodyArmorIndex       = Ref Constants.noParent 0
                   , _giPowerScreenIndex     = Ref Constants.noParent 0
                   , _giPowerShieldIndex     = Ref Constants.noParent 0
                   }