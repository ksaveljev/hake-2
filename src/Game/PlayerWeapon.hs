{-# LANGUAGE OverloadedStrings #-}
module Game.PlayerWeapon where

import Game.EntInteractAdapter
import Game.EntThinkAdapter
import Game.ItemDropAdapter
import Game.ItemUseAdapter

useWeapon :: ItemUseAdapter
useWeapon =
  ItemUseAdapter { _iuaId = "Use_Weapon"
                 , _use = undefined -- TODO
                 }

weaponBlaster :: EntThinkAdapter
weaponBlaster =
  EntThinkAdapter { _etaId = "Weapon_Blaster"
                  , _think = undefined -- TODO
                  }

pickupWeapon :: EntInteractAdapter
pickupWeapon =
  EntInteractAdapter { _eiaId = "Pickup_Weapon"
                     , _interact = undefined -- TODO
                     }

dropWeapon :: ItemDropAdapter
dropWeapon =
  ItemDropAdapter { _idaId = "Drop_Weapon"
                  , _drop = undefined -- TODO
                  }

weaponShotgun :: EntThinkAdapter
weaponShotgun =
  EntThinkAdapter { _etaId = "Weapon_Shotgun"
                  , _think = undefined -- TODO
                  }

weaponSuperShotgun :: EntThinkAdapter
weaponSuperShotgun = 
  EntThinkAdapter { _etaId = "Weapon_SuperShotgun"
                  , _think = undefined -- TODO
                  }

weaponMachinegun :: EntThinkAdapter
weaponMachinegun = 
  EntThinkAdapter { _etaId = "Weapon_Machinegun"
                  , _think = undefined -- TODO
                  }

weaponChaingun :: EntThinkAdapter
weaponChaingun = 
  EntThinkAdapter { _etaId = "Weapon_Chaingun"
                  , _think = undefined -- TODO
                  }

weaponGrenade :: EntThinkAdapter
weaponGrenade = 
  EntThinkAdapter { _etaId = "Weapon_Grenade"
                  , _think = undefined -- TODO
                  }

weaponGrenadeLauncher :: EntThinkAdapter
weaponGrenadeLauncher = 
  EntThinkAdapter { _etaId = "Weapon_GrenadeLauncher"
                  , _think = undefined -- TODO
                  }

weaponRocketLauncher :: EntThinkAdapter
weaponRocketLauncher = 
  EntThinkAdapter { _etaId = "Weapon_RocketLauncher"
                  , _think = undefined -- TODO
                  }

weaponHyperBlaster :: EntThinkAdapter
weaponHyperBlaster = 
  EntThinkAdapter { _etaId = "Weapon_HyperBlaster"
                  , _think = undefined -- TODO
                  }

weaponRailgun :: EntThinkAdapter
weaponRailgun = 
  EntThinkAdapter { _etaId = "Weapon_Railgun"
                  , _think = undefined -- TODO
                  }

weaponBFG :: EntThinkAdapter
weaponBFG = 
  EntThinkAdapter { _etaId = "Weapon_BFG"
                  , _think = undefined -- TODO
                  }
