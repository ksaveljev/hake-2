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
  GenericEntThink { _getId = "Weapon_Blaster"
                  , _getThink = undefined -- TODO
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
  GenericEntThink { _getId = "Weapon_Shotgun"
                  , _getThink = undefined -- TODO
                  }

weaponSuperShotgun :: EntThink
weaponSuperShotgun = 
  GenericEntThink { _getId = "Weapon_SuperShotgun"
                  , _getThink = undefined -- TODO
                  }

weaponMachinegun :: EntThink
weaponMachinegun = 
  GenericEntThink { _getId = "Weapon_Machinegun"
                  , _getThink = undefined -- TODO
                  }

weaponChaingun :: EntThink
weaponChaingun = 
  GenericEntThink { _getId = "Weapon_Chaingun"
                  , _getThink = undefined -- TODO
                  }

weaponGrenade :: EntThink
weaponGrenade = 
  GenericEntThink { _getId = "Weapon_Grenade"
                  , _getThink = undefined -- TODO
                  }

weaponGrenadeLauncher :: EntThink
weaponGrenadeLauncher = 
  GenericEntThink { _getId = "Weapon_GrenadeLauncher"
                  , _getThink = undefined -- TODO
                  }

weaponRocketLauncher :: EntThink
weaponRocketLauncher = 
  GenericEntThink { _getId = "Weapon_RocketLauncher"
                  , _getThink = undefined -- TODO
                  }

weaponHyperBlaster :: EntThink
weaponHyperBlaster = 
  GenericEntThink { _getId = "Weapon_HyperBlaster"
                  , _getThink = undefined -- TODO
                  }

weaponRailgun :: EntThink
weaponRailgun = 
  GenericEntThink { _getId = "Weapon_Railgun"
                  , _getThink = undefined -- TODO
                  }

weaponBFG :: EntThink
weaponBFG = 
  GenericEntThink { _getId = "Weapon_BFG"
                  , _getThink = undefined -- TODO
                  }
