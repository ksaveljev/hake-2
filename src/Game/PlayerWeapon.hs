{-# LANGUAGE OverloadedStrings #-}
module Game.PlayerWeapon where

import Game.EntInteract
import Game.EntThink
import Game.ItemDrop
import Game.ItemUse

useWeapon :: ItemUse
useWeapon =
  GenericItemUse { _giuId = "Use_Weapon"
                 , _giuUse = undefined -- TODO
                 }

weaponBlaster :: EntThink
weaponBlaster =
  GenericEntThink { _gethId = "Weapon_Blaster"
                  , _gethThink = undefined -- TODO
                  }

pickupWeapon :: EntInteract
pickupWeapon =
  GenericEntInteract { _geiId = "Pickup_Weapon"
                     , _geiInteract = undefined -- TODO
                     }

dropWeapon :: ItemDrop
dropWeapon =
  GenericItemDrop { _gidId = "Drop_Weapon"
                  , _gidDrop = undefined -- TODO
                  }

weaponShotgun :: EntThink
weaponShotgun =
  GenericEntThink { _gethId = "Weapon_Shotgun"
                  , _gethThink = undefined -- TODO
                  }

weaponSuperShotgun :: EntThink
weaponSuperShotgun = 
  GenericEntThink { _gethId = "Weapon_SuperShotgun"
                  , _gethThink = undefined -- TODO
                  }

weaponMachinegun :: EntThink
weaponMachinegun = 
  GenericEntThink { _gethId = "Weapon_Machinegun"
                  , _gethThink = undefined -- TODO
                  }

weaponChaingun :: EntThink
weaponChaingun = 
  GenericEntThink { _gethId = "Weapon_Chaingun"
                  , _gethThink = undefined -- TODO
                  }

weaponGrenade :: EntThink
weaponGrenade = 
  GenericEntThink { _gethId = "Weapon_Grenade"
                  , _gethThink = undefined -- TODO
                  }

weaponGrenadeLauncher :: EntThink
weaponGrenadeLauncher = 
  GenericEntThink { _gethId = "Weapon_GrenadeLauncher"
                  , _gethThink = undefined -- TODO
                  }

weaponRocketLauncher :: EntThink
weaponRocketLauncher = 
  GenericEntThink { _gethId = "Weapon_RocketLauncher"
                  , _gethThink = undefined -- TODO
                  }

weaponHyperBlaster :: EntThink
weaponHyperBlaster = 
  GenericEntThink { _gethId = "Weapon_HyperBlaster"
                  , _gethThink = undefined -- TODO
                  }

weaponRailgun :: EntThink
weaponRailgun = 
  GenericEntThink { _gethId = "Weapon_Railgun"
                  , _gethThink = undefined -- TODO
                  }

weaponBFG :: EntThink
weaponBFG = 
  GenericEntThink { _gethId = "Weapon_BFG"
                  , _gethThink = undefined -- TODO
                  }
