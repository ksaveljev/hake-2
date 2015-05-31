{-# LANGUAGE OverloadedStrings #-}
module Game.PlayerWeapon where

import Control.Lens (use, preuse, ix, (.=), (^.), zoom)
import Control.Monad (when)
import Data.Bits ((.&.), (.|.), shiftL)
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import Game.Adapters

useWeapon :: ItemUse
useWeapon =
  GenericItemUse "Use_Weapon" $ \_ _ -> do
    io (putStrLn "PlayerWeapon.useWeapon") >> undefined -- TODO

weaponBlaster :: EntThink
weaponBlaster =
  GenericEntThink "Weapon_Blaster" $ \edictRef -> do
    let pauseFrames = UV.fromList [19, 32, 0]
        fireFrames = UV.fromList [5, 0]

    weaponGeneric edictRef 4 8 52 55 pauseFrames fireFrames weaponBlasterFire
    return True

weaponBlasterFire :: EntThink
weaponBlasterFire =
  GenericEntThink "Weapon_Blaster_Fire" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponBlasterFire") >> undefined -- TODO

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
  GenericEntThink "Weapon_Shotgun" $ \edictRef -> do
    let pauseFrames = UV.fromList [22, 28, 34, 0]
        fireFrames = UV.fromList [8, 9, 0]

    weaponGeneric edictRef 7 18 36 39 pauseFrames fireFrames weaponShotgunFire
    return True

weaponShotgunFire :: EntThink
weaponShotgunFire =
  GenericEntThink "weapon_shotgun_fire" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponShotgunFire") >> undefined -- TODO

weaponSuperShotgun :: EntThink
weaponSuperShotgun = 
  GenericEntThink "Weapon_SuperShotgun" $ \edictRef -> do
    let pauseFrames = UV.fromList [29, 42, 57, 0]
        fireFrames = UV.fromList [7, 0]

    weaponGeneric edictRef 6 17 57 61 pauseFrames fireFrames weaponSuperShotgunFire
    return True

weaponSuperShotgunFire :: EntThink
weaponSuperShotgunFire =
  GenericEntThink "weapon_supershotgun_fire" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponSuperShotgunFire") >> undefined -- TODO

weaponMachinegun :: EntThink
weaponMachinegun = 
  GenericEntThink "Weapon_Machinegun" $ \edictRef -> do
    let pauseFrames = UV.fromList [23, 45, 0]
        fireFrames = UV.fromList [4, 5, 0]

    weaponGeneric edictRef 3 5 45 49 pauseFrames fireFrames machinegunFire
    return True

machinegunFire :: EntThink
machinegunFire =
  GenericEntThink "Machinegun_Fire" $ \_ -> do
    io (putStrLn "PlayerWeapon.machinegunFire") >> undefined -- TODO

weaponChaingun :: EntThink
weaponChaingun = 
  GenericEntThink "Weapon_Chaingun" $ \edictRef -> do
    let pauseFrames = UV.fromList [38, 43, 51, 61, 0]
        fireFrames = UV.fromList [5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 0]

    weaponGeneric edictRef 4 31 61 64 pauseFrames fireFrames chaingunFire
    return True

chaingunFire :: EntThink
chaingunFire =
  GenericEntThink "Chaingun_Fire" $ \_ -> do
    io (putStrLn "PlayerWeapon.chaingunFire") >> undefined -- TODO

weaponGrenade :: EntThink
weaponGrenade = 
  GenericEntThink "Weapon_Grenade" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponGrenade") >> undefined -- TODO

weaponGrenadeLauncher :: EntThink
weaponGrenadeLauncher = 
  GenericEntThink "Weapon_GrenadeLauncher" $ \edictRef -> do
    let pauseFrames = UV.fromList [34, 51, 59, 0]
        fireFrames = UV.fromList [6, 0]

    weaponGeneric edictRef 5 16 59 64 pauseFrames fireFrames weaponGrenadeLauncherFire
    return True

weaponGrenadeLauncherFire :: EntThink
weaponGrenadeLauncherFire =
  GenericEntThink "weapon_grenadelauncher_fire" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponGrenadeLauncherFire") >> undefined -- TODO

weaponRocketLauncher :: EntThink
weaponRocketLauncher = 
  GenericEntThink "Weapon_RocketLauncher" $ \edictRef -> do
    let pauseFrames = UV.fromList [25, 33, 42, 50, 0]
        fireFrames = UV.fromList [5, 0]

    weaponGeneric edictRef 4 12 50 54 pauseFrames fireFrames weaponRocketLauncherFire
    return True

weaponRocketLauncherFire :: EntThink
weaponRocketLauncherFire =
  GenericEntThink "Weapon_RocketLauncher_Fire" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponRocketLauncherFire") >> undefined -- TODO

weaponHyperBlaster :: EntThink
weaponHyperBlaster = 
  GenericEntThink "Weapon_HyperBlaster" $ \edictRef -> do
    let pauseFrames = UV.fromList [0]
        fireFrames = UV.fromList [6, 7, 8, 9, 10, 11, 0]

    weaponGeneric edictRef 5 20 49 53 pauseFrames fireFrames weaponHyperBlasterFire
    return True

weaponHyperBlasterFire :: EntThink
weaponHyperBlasterFire =
  GenericEntThink "Weapon_HyperBlaster_Fire" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponHyperBlasterFire") >> undefined -- TODO

weaponRailgun :: EntThink
weaponRailgun = 
  GenericEntThink "Weapon_Railgun" $ \edictRef -> do
    let pauseFrames = UV.fromList [56, 0]
        fireFrames = UV.fromList [4, 0]

    weaponGeneric edictRef 3 18 56 61 pauseFrames fireFrames weaponRailgunFire
    io (putStrLn "PlayerWeapon.weaponRailgun") >> undefined -- TODO

weaponRailgunFire :: EntThink
weaponRailgunFire =
  GenericEntThink "weapon_railgun_fire" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponRailgunFire") >> undefined -- TODO

weaponBFG :: EntThink
weaponBFG =
  GenericEntThink "Weapon_BFG" $ \edictRef -> do
    let pauseFrames = UV.fromList [39, 45, 50, 55, 0]
        fireFrames = UV.fromList [9, 17, 0]

    weaponGeneric edictRef 8 32 55 58 pauseFrames fireFrames weaponBFGFire
    return True

weaponBFGFire :: EntThink
weaponBFGFire =
  GenericEntThink "weapon_bfg_fire" $ \_ -> do
    io (putStrLn "PlayerWeapon.weaponBFGFire") >> undefined -- TODO

changeWeapon :: EdictReference -> Quake ()
changeWeapon edictRef@(EdictReference edictIdx) = do
    Just (Just gClientRef@(GClientReference gClientIdx)) <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eClient

    checkGrenadeTime gClientRef

    preuse (gameBaseGlobals.gbGame.glClients.ix gClientIdx) >>= \(Just gClient) -> 
      zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
        gcPers.cpLastWeapon .= gClient^.gcPers.cpWeapon
        gcPers.cpWeapon .= gClient^.gcNewWeapon
        gcNewWeapon .= Nothing
        gcMachinegunShots .= 0

    -- set visible model
    setVisibleModel gClientRef

    io (putStrLn "PlayerWeapon.changeWeapon") >> undefined -- TODO

  where checkGrenadeTime :: GClientReference -> Quake ()
        checkGrenadeTime (GClientReference gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          when (gClient^.gcGrenadeTime /= 0) $ do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcGrenadeTime .= levelTime
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcWeaponSound .= 0
            weaponGrenadeFire edictRef False
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcGrenadeTime .= 0

        setVisibleModel :: GClientReference -> Quake ()
        setVisibleModel (GClientReference gClientIdx) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          when ((edict^.eEntityState.esModelIndex) == 255) $ do
            Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

            i <- case gClient^.gcPers.cpWeapon of
                   Nothing -> return 0
                   Just (GItemReference weaponIdx) -> do
                     Just weaponModel <- preuse $ gameBaseGlobals.gbItemList.ix weaponIdx.giWeaponModel
                     return $ (weaponModel .&. 0xFF) `shiftL` 8

            gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esSkinNum .= (edictIdx - 1) .|. i

thinkWeapon :: EdictReference -> Quake ()
thinkWeapon _ = do
    io (putStrLn "PlayerWeapon.thinkWeapon") >> undefined -- TODO

weaponGeneric :: EdictReference -> Int -> Int -> Int -> Int -> UV.Vector Int -> UV.Vector Int -> EntThink -> Quake ()
weaponGeneric _ _ _ _ _ _ _ _ = do
    io (putStrLn "PlayerWeapon.weaponGeneric") >> undefined -- TODO

weaponGrenadeFire :: EdictReference -> Bool -> Quake ()
weaponGrenadeFire _ _ = do
    io (putStrLn "PlayerWeapon.weaponGrenadeFire") >> undefined -- TODO
