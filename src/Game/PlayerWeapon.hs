{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Game.PlayerWeapon where

import Control.Lens (use, preuse, ix, (.=), (^.), zoom, (+=), (-=))
import Control.Monad (when, liftM, void, unless)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Maybe (isJust, fromJust)
import Linear (V3)
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameItems as GameItems
import qualified Game.Monsters.MPlayer as MPlayer

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
  GenericEntThink "Weapon_Blaster_Fire" $ \edictRef@(EdictReference edictIdx) -> do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    let damage = if deathmatchValue /= 0 then 15 else 10
    v3o <- use $ globals.vec3Origin

    blasterFire edictRef v3o damage False Constants.efBlaster

    Just (Just (GClientReference gClientIdx)) <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eClient
    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

    return True

pickupWeapon :: EntInteract
pickupWeapon =
  GenericEntInteract "Pickup_Weapon" $ \_ _ -> do
    io (putStrLn "PlayerWeapon.pickupWeapon") >> undefined -- TODO

dropWeapon :: ItemDrop
dropWeapon =
  GenericItemDrop "Drop_Weapon" $ \edictRef@(EdictReference edictIdx) itemRef@(GItemReference gItemIdx) -> do
    dmFlagsValue :: Int <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    when (dmFlagsValue .&. Constants.dfWeaponsStay == 0) $ do
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      Just (Just (GClientReference gClientIdx)) <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eClient
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

      let index = gItem^.giIndex

      if ((Just itemRef) == (gClient^.gcPers.cpWeapon) || (Just itemRef) == (gClient^.gcNewWeapon)) && ((gClient^.gcPers.cpInventory) UV.! index) == 1
        then do
          cprintf <- use $ gameBaseGlobals.gbGameImport.giCprintf
          cprintf edictRef Constants.printHigh "Can't drop current weapon\n"
        else do
          void $ GameItems.dropItem edictRef itemRef
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix index -= 1

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

    done <- setAmmoAndGunIndex gClientRef

    unless done $ do
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimPriority .= Constants.animPain
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

      if fromIntegral (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
        then do
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.frameCRPain1
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameCRPain4
        else do
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.framePain301
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.framePain304

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

        setAmmoAndGunIndex :: GClientReference -> Quake Bool
        setAmmoAndGunIndex (GClientReference gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          case gClient^.gcPers.cpWeapon of
            Nothing -> do
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAmmoIndex .= 0

            Just (GItemReference weaponIdx) -> do
              Just weapon <- preuse $ gameBaseGlobals.gbItemList.ix weaponIdx

              case weapon^.giAmmo of
                Nothing -> do
                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAmmoIndex .= 0

                Just ammo -> do
                  Just (GItemReference ammoItemIdx) <- GameItems.findItem ammo
                  Just ammoItem <- preuse $ gameBaseGlobals.gbItemList.ix ammoItemIdx
                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAmmoIndex .= ammoItem^.giIndex

          case gClient^.gcPers.cpWeapon of
            Nothing -> do -- dead
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunIndex .= 0
              return True

            Just (GItemReference weaponIdx) -> do
              Just weapon <- preuse $ gameBaseGlobals.gbItemList.ix weaponIdx
              modelIndex <- use $ gameBaseGlobals.gbGameImport.giModelIndex
              gunIndex <- modelIndex (weapon^.giViewModel)

              zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
                gcWeaponState .= Constants.weaponActivating
                gcPlayerState.psGunFrame .= 0
                gcPlayerState.psGunIndex .= gunIndex

              return False

{-
- ================= 
- Think_Weapon
- 
- Called by ClientBeginServerFrame and ClientThink 
- =================
-}
thinkWeapon :: EdictReference -> Quake ()
thinkWeapon edictRef@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let Just (GClientReference gClientIdx) = edict^.eClient

    -- if just died, put the weapon away
    when ((edict^.eEdictStatus.eHealth) < 1) $ do
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcNewWeapon .= Nothing
      changeWeapon edictRef

    -- call active weapon think routine
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    when (isJust (gClient^.gcPers.cpWeapon)) $ do
      let Just (GItemReference weaponIdx) = gClient^.gcPers.cpWeapon
      Just weapon <- preuse $ gameBaseGlobals.gbItemList.ix weaponIdx

      when (isJust (weapon^.giWeaponThink)) $ do
        frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

        gameBaseGlobals.gbIsQuad .= (truncate (gClient^.gcQuadFrameNum) > frameNum)
        gameBaseGlobals.gbIsSilenced .= if (gClient^.gcSilencerShots) /= 0
                                          then Constants.mzSilenced
                                          else 0
        
        void $ think (fromJust $ weapon^.giWeaponThink) edictRef

weaponGeneric :: EdictReference -> Int -> Int -> Int -> Int -> UV.Vector Int -> UV.Vector Int -> EntThink -> Quake ()
weaponGeneric edictRef@(EdictReference edictIdx) frameActiveLast frameFirstLast frameIdleLast frameDeactivateLast pauseFrames fireFrames fire = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    if | (edict^.eEdictStatus.eDeadFlag) /= 0 || (edict^.eEntityState.esModelIndex) /= 255 ->
           return () -- VWep animations screw up corpses

       | (gClient^.gcWeaponState) == Constants.weaponDropping -> do
           if | (gClient^.gcPlayerState.psGunFrame) == frameDeactivateLast ->
                  changeWeapon edictRef

              | frameDeactivateLast - (gClient^.gcPlayerState.psGunFrame) == 4 -> do
                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimPriority .= Constants.animReverse

                  if fromIntegral (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
                    then do
                      gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.frameCRPain4 + 1
                      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameCRPain1
                    else do
                      gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.framePain304 + 1
                      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.framePain301

                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

              | otherwise ->
                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

       | (gClient^.gcWeaponState) == Constants.weaponActivating -> do
           undefined -- TODO

       | isJust (gClient^.gcNewWeapon) && (gClient^.gcWeaponState) /= Constants.weaponFiring -> do
           undefined -- TODO

       | otherwise -> do
           io (putStrLn "PlayerWeapon.weaponGeneric") >> undefined -- TODO

weaponGrenadeFire :: EdictReference -> Bool -> Quake ()
weaponGrenadeFire _ _ = do
    io (putStrLn "PlayerWeapon.weaponGrenadeFire") >> undefined -- TODO

blasterFire :: EdictReference -> V3 Float -> Int -> Bool -> Int -> Quake ()
blasterFire _ _ _ _ _ = do
    io (putStrLn "PlayerWeapon.blasterFire") >> undefined -- TODO

playerNoise :: EdictReference -> V3 Float -> Int -> Quake ()
playerNoise _ _ _ = do
    io (putStrLn "PlayerWeapon.playerNoise") >> undefined -- TODO
