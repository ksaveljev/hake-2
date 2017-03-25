{-# LANGUAGE TemplateHaskell #-}
module Game.GameItemsGlobals
    ( module Game.GameItemsGlobals
    ) where

import           Control.Lens (makeLenses)

import qualified Constants
import           Types

makeLenses ''GameItemsGlobals

initialGameItemsGlobals :: GameItemsGlobals
initialGameItemsGlobals = GameItemsGlobals
    { _giJacketArmorInfo      = GItemArmorT 25 50 0.30 0 Constants.armorJacket
    , _giCombatArmorInfo      = GItemArmorT 50 100 0.60 0.30 Constants.armorCombat
    , _giBodyArmorInfo        = GItemArmorT 100 200 0.80 0.60 Constants.armorBody
    , _giQuakeDropTimeoutHack = 0
    , _giJacketArmorIndex     = Ref 0
    , _giCombatArmorIndex     = Ref 0
    , _giBodyArmorIndex       = Ref 0
    , _giPowerScreenIndex     = Ref 0
    , _giPowerShieldIndex     = Ref 0
    }