{-# LANGUAGE OverloadedStrings #-}
module Game.PlayerWeapon where

import Quake
import QuakeState
import Game.Adapters

useWeapon :: ItemUse
useWeapon =
  GenericItemUse "Use_Weapon" $ \_ _ -> do
    io (putStrLn "PlayerWeapon.useWeapon") >> undefined -- TODO

weaponBlaster :: EntThink
weaponBlaster =
  GenericEntThink "Weapon_Blaster" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponBlaster") >> undefined -- TODO

pickupWeapon :: EntInteract
pickupWeapon =
  GenericEntInteract "Pickup_Weapon" $ \_ _ -> do
    io (putStrLn "PlayerWeapon.pickupWeapon") >> undefined -- TODO

dropWeapon :: ItemDrop
dropWeapon =
  GenericItemDrop "Drop_Weapon" $ \_ _ -> do
    io (putStrLn "PlayerWeapon.dropWeapon") >> undefined -- TODO

weaponShotgun :: EntThink
weaponShotgun =
  GenericEntThink "Weapon_Shotgun" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponShotgun") >> undefined -- TODO

weaponSuperShotgun :: EntThink
weaponSuperShotgun = 
  GenericEntThink "Weapon_SuperShotgun" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponSuperShotgun") >> undefined -- TODO

weaponMachinegun :: EntThink
weaponMachinegun = 
  GenericEntThink "Weapon_Machinegun" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponMachinegun") >> undefined -- TODO

weaponChaingun :: EntThink
weaponChaingun = 
  GenericEntThink "Weapon_Chaingun" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponChaingun") >> undefined -- TODO

weaponGrenade :: EntThink
weaponGrenade = 
  GenericEntThink "Weapon_Grenade" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponGrenade") >> undefined -- TODO

weaponGrenadeLauncher :: EntThink
weaponGrenadeLauncher = 
  GenericEntThink "Weapon_GrenadeLauncher" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponGrenadeLauncher") >> undefined -- TODO

weaponRocketLauncher :: EntThink
weaponRocketLauncher = 
  GenericEntThink "Weapon_RocketLauncher" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponRocketLauncher") >> undefined -- TODO

weaponHyperBlaster :: EntThink
weaponHyperBlaster = 
  GenericEntThink "Weapon_HyperBlaster" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponHyperBlaster") >> undefined -- TODO

weaponRailgun :: EntThink
weaponRailgun = 
  GenericEntThink "Weapon_Railgun" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponRailgun") >> undefined -- TODO

weaponBFG :: EntThink
weaponBFG =
  GenericEntThink "Weapon_BFG" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponBFG") >> undefined -- TODO

changeWeapon :: EdictReference -> Quake ()
changeWeapon _ = do
    io (putStrLn "PlayerWeapon.changeWeapon") >> undefined -- TODO
