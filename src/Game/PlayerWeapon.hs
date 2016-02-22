module Game.PlayerWeapon
  ( dropWeapon
  , pickupWeapon
  , useWeapon
  , weaponBFG
  , weaponBlaster
  , weaponChaingun
  , weaponGrenade
  , weaponGrenadeLauncher
  , weaponHyperBlaster
  , weaponMachinegun
  , weaponRailgun
  , weaponRocketLauncher
  , weaponShotgun
  , weaponSuperShotgun
  ) where

import Types

useWeapon :: ItemUse
useWeapon = error "PlayerWeapon.useWeapon" -- TODO

pickupWeapon :: EntInteract
pickupWeapon = error "PlayerWeapon.pickupWeapon" -- TODO

dropWeapon :: ItemDrop
dropWeapon = error "PlayerWeapon.dropWeapon" -- TODO

weaponBlaster :: EntThink
weaponBlaster = error "PlayerWeapon.weaponBlaster" -- TODO

weaponShotgun :: EntThink
weaponShotgun = error "PlayerWeapon.weaponShotgun" -- TODO

weaponSuperShotgun :: EntThink
weaponSuperShotgun = error "PlayerWeapon.weaponShotgun" -- TODO

weaponMachinegun :: EntThink
weaponMachinegun = error "PlayerWeapon.weaponMachinegun" -- TODO

weaponChaingun :: EntThink
weaponChaingun = error "PlayerWeapon.weaponChaingun" -- TODO

weaponGrenade :: EntThink
weaponGrenade = error "PlayerWeapon.weaponGrenade" -- TODO

weaponGrenadeLauncher :: EntThink
weaponGrenadeLauncher = error "PlayerWeapon.weaponGrenadeLauncher" -- TODO

weaponRocketLauncher :: EntThink
weaponRocketLauncher = error "PlayerWeapon.weaponRocketLauncher" -- TODO

weaponHyperBlaster :: EntThink
weaponHyperBlaster = error "PlayerWeapon.weaponHyperBlaster" -- TODO

weaponRailgun :: EntThink
weaponRailgun = error "PlayerWeapon.weaponRailgun" -- TODO

weaponBFG :: EntThink
weaponBFG = error "PlayerWeapon.weaponBFG" -- TODO