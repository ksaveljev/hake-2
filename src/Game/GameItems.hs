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

pickupAmmo :: EntInteractAdapter
pickupAmmo =
  EntInteractAdapter { _eiaId = "pickup_ammo"
                     , _interact = undefined -- TODO
                     }

dropAmmo :: ItemDropAdapter
dropAmmo =
  ItemDropAdapter { _idaId = "drop_ammo"
                  , _drop = undefined -- TODO
                  }

useQuad :: ItemUseAdapter
useQuad =
  ItemUseAdapter { _iuaId = "use_quad"
                 , _use = undefined -- TODO
                 }

pickupPowerup :: EntInteractAdapter
pickupPowerup =
  EntInteractAdapter { _eiaId = "pickup_powerup"
                     , _interact = undefined -- TODO
                     }

dropGeneral :: ItemDropAdapter
dropGeneral =
  ItemDropAdapter { _idaId = "drop_general"
                  , _drop = undefined -- TODO
                  }

useInvulnerability :: ItemUseAdapter
useInvulnerability =
  ItemUseAdapter { _iuaId = "use_invulnerability"
                 , _use = undefined -- TODO
                 }

useSilencer :: ItemUseAdapter
useSilencer =
  ItemUseAdapter { _iuaId = "use_silencer"
                 , _use = undefined -- TODO
                 }

useBreather :: ItemUseAdapter
useBreather =
  ItemUseAdapter { _iuaId = "use_breather"
                 , _use = undefined -- TODO
                 }

useEnviroSuit :: ItemUseAdapter
useEnviroSuit =
  ItemUseAdapter { _iuaId = "use_envirosuit"
                 , _use = undefined -- TODO
                 }

pickupAncientHead :: EntInteractAdapter
pickupAncientHead =
  EntInteractAdapter { _eiaId = "pickup_ancienthead"
                     , _interact = undefined -- TODO
                     }

pickupAdrenaline :: EntInteractAdapter
pickupAdrenaline =
  EntInteractAdapter { _eiaId = "pickup_adrenaline"
                     , _interact = undefined -- TODO
                     }

pickupBandolier :: EntInteractAdapter
pickupBandolier =
  EntInteractAdapter { _eiaId = "pickup_bandolier"
                     , _interact = undefined -- TODO
                     }

pickupPack :: EntInteractAdapter
pickupPack =
  EntInteractAdapter { _eiaId = "pickup_pack"
                     , _interact = undefined -- TODO
                     }

pickupKey :: EntInteractAdapter
pickupKey =
  EntInteractAdapter { _eiaId = "pickup_key"
                     , _interact = undefined -- TODO
                     }

pickupHealth :: EntInteractAdapter
pickupHealth =
  EntInteractAdapter { _eiaId = "pickup_health"
                     , _interact = undefined -- TODO
                     }
