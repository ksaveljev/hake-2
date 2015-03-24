{-# LANGUAGE OverloadedStrings #-}
module Game.GameItems where

import Quake
import Game.EntInteractAdapter
import Game.GItemArmorT
import Game.ItemDropAdapter
import Game.ItemUseAdapter
import qualified Constants

initItems :: Quake ()
initItems = io (putStrLn "GameItems.initItems") >> undefined -- TODO

jacketArmorInfo :: GItemArmorT
jacketArmorInfo =
  GItemArmorT { _giaBaseCount        = 25
              , _giaMaxCount         = 50
              , _giaNormalProtection = 0.3
              , _giaEnergyProteciton = 0
              , _giaArmor            = Constants.armorJacket
              }

combatArmorInfo :: GItemArmorT
combatArmorInfo =
  GItemArmorT { _giaBaseCount        = 50
              , _giaMaxCount         = 100
              , _giaNormalProtection = 0.6
              , _giaEnergyProteciton = 0.3
              , _giaArmor            = Constants.armorCombat
              }

bodyArmorInfo :: GItemArmorT
bodyArmorInfo =
  GItemArmorT { _giaBaseCount        = 100
              , _giaMaxCount         = 200
              , _giaNormalProtection = 0.8
              , _giaEnergyProteciton = 0.6
              , _giaArmor            = Constants.armorBody
              }

pickupArmor :: EntInteractAdapter
pickupArmor =
  EntInteractAdapter { _eiaId = "pickup_armor"
                     , _interact = undefined -- TODO
                     }

pickupPowerArmor :: EntInteractAdapter
pickupPowerArmor =
  EntInteractAdapter { _eiaId = "pickup_powerarmor"
                     , _interact = undefined -- TODO
                     }

usePowerArmor :: ItemUseAdapter
usePowerArmor =
  ItemUseAdapter { _iuaId = "use_powerarmor"
                 , _use = undefined -- TODO
                 }

dropPowerArmor :: ItemDropAdapter
dropPowerArmor =
  ItemDropAdapter { _idaId = "drop_powerarmor"
                  , _drop = undefined -- TODO
                  }
