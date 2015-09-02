{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Game.PlayerWeapon where

import Control.Lens (use, preuse, ix, (.=), (^.), zoom, (+=), (-=), (%=))
import Control.Monad (when, liftM, void, unless)
import Data.Bits ((.&.), (.|.), shiftL, complement)
import Data.Maybe (isJust, isNothing, fromJust)
import Linear (V3(..), _x)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameItems as GameItems
import qualified Game.GameUtil as GameUtil
import qualified Game.GameWeapon as GameWeapon
import qualified Game.Monsters.MPlayer as MPlayer
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

{-
- ================ 
- Use_Weapon
- 
- Make the weapon ready if there is ammo 
- ================
-}
useWeapon :: ItemUse
useWeapon =
  GenericItemUse "Use_Weapon" $ \edictRef@(EdictReference edictIdx) gItemRef@(GItemReference gItemIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    -- see if we're already using it
    unless (Just gItemRef == (gClient^.gcPers.cpWeapon)) $ do
      Just item <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      gSelectEmptyValue <- liftM (^.cvValue) gSelectEmptyCVar

      if isJust (item^.giAmmo) && gSelectEmptyValue == 0 && (item^.giFlags) .&. Constants.itAmmo == 0
        then do
          Just (GItemReference ammoItemIdx) <- GameItems.findItem (fromJust (item^.giAmmo))
          Just ammoItem <- preuse $ gameBaseGlobals.gbItemList.ix ammoItemIdx
          let ammoIndex = ammoItem^.giIndex
              quantity = (gClient^.gcPers.cpInventory) UV.! ammoIndex

          cprintf <- use $ gameBaseGlobals.gbGameImport.giCprintf

          if | quantity == 0 ->
                 cprintf edictRef Constants.printHigh ("No " `B.append` (fromJust $ ammoItem^.giPickupName) `B.append` " for " `B.append` (fromJust $ item^.giPickupName) `B.append` ".\n")

             | quantity < (item^.giQuantity) -> do
                 cprintf edictRef Constants.printHigh ("Not enough " `B.append` (fromJust $ ammoItem^.giPickupName) `B.append` " for " `B.append` (fromJust $ item^.giPickupName) `B.append` ".\n")

             | otherwise ->
                 gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcNewWeapon .= Just gItemRef

        else
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcNewWeapon .= Just gItemRef

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
  GenericEntInteract "Pickup_Weapon" $ \edictRef@(EdictReference edictIdx) otherRef@(EdictReference otherIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    Just other <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx
    let Just (GItemReference itemIdx) = edict^.eItem
    Just item <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx
    let index = item^.giIndex

    dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar
    coopValue <- liftM (^.cvValue) coopCVar
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    let Just (GClientReference otherClientIdx) = other^.eClient
    Just otherClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix otherClientIdx

    let done = (dmFlagsValue .&. Constants.dfWeaponsStay /= 0 || coopValue /= 0) && (otherClient^.gcPers.cpInventory) UV.! index /= 0 && (edict^.eSpawnFlags) .&. (Constants.droppedItem .|. Constants.droppedPlayerItem) == 0

    if done
      then
        return False -- leave the weapon for others to pickup
      else do
        gameBaseGlobals.gbGame.glClients.ix otherClientIdx.gcPers.cpInventory.ix index += 1

        when ((edict^.eSpawnFlags) .&. Constants.droppedItem == 0) $ do
          -- give them some ammo with it
          Just ammoRef@(GItemReference ammoIdx) <- GameItems.findItem (fromJust $ item^.giAmmo)
          Just ammo <- preuse $ gameBaseGlobals.gbItemList.ix ammoIdx

          void $ if dmFlagsValue .&. Constants.dfInfiniteAmmo /= 0
                   then GameItems.addAmmo otherRef ammoRef 1000
                   else GameItems.addAmmo otherRef ammoRef (ammo^.giQuantity)

          when ((edict^.eSpawnFlags) .&. Constants.droppedPlayerItem == 0) $ do
            when (deathmatchValue /= 0) $
              if dmFlagsValue .&. Constants.dfWeaponsStay /= 0
                then gameBaseGlobals.gbGEdicts.ix edictIdx.eFlags %= (.|. Constants.flRespawn)
                else GameItems.setRespawn edictRef 30

            when (coopValue /= 0) $
              gameBaseGlobals.gbGEdicts.ix edictIdx.eFlags %= (.|. Constants.flRespawn)

        Just edict' <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
        Just otherClient' <- preuse $ gameBaseGlobals.gbGame.glClients.ix otherClientIdx

        blasterIdx <- GameItems.findItem "blaster"

        when ((otherClient'^.gcPers.cpWeapon) /= (edict'^.eItem) && (otherClient'^.gcPers.cpInventory) UV.! index == 1 && (deathmatchValue == 0 || (otherClient'^.gcPers.cpWeapon) == blasterIdx)) $
          gameBaseGlobals.gbGame.glClients.ix otherClientIdx.gcNewWeapon .= (edict'^.eItem)

        return True

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
  GenericEntThink "weapon_shotgun_fire" $ \edictRef@(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just client <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    if (client^.gcPlayerState.psGunFrame) == 9
      then do
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1
        return True
      else do
        let (Just forward, Just right, _) = Math3D.angleVectors (client^.gcVAngle) True True False
            kickOrigin = fmap (* (-2)) forward
            offset = V3 0 8 (fromIntegral (edict^.eViewHeight) - 8)

        zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
          gcKickOrigin .= kickOrigin
          gcKickAngles._x .= (-2)

        isQuad <- use $ gameBaseGlobals.gbIsQuad
        isSilenced <- use $ gameBaseGlobals.gbIsSilenced
        gameImport <- use $ gameBaseGlobals.gbGameImport
        deathmatchValue <- liftM (^.cvValue) deathmatchCVar
        dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar

        let start = projectSource client (edict^.eEntityState.esOrigin) offset forward right
            damage = if isQuad then 4 * 4 else 4
            kick = if isQuad then 8 * 4 else 8
            writeByte = gameImport^.giWriteByte
            writeShort = gameImport^.giWriteShort
            multicast = gameImport^.giMulticast

        if deathmatchValue /= 0
          then GameWeapon.fireShotgun edictRef start forward damage kick 500 500 Constants.defaultDeathmatchShotgunCount Constants.modShotgun
          else GameWeapon.fireShotgun edictRef start forward damage kick 500 500 Constants.defaultShotgunCount Constants.modShotgun

        -- send muzzle flash
        writeByte Constants.svcMuzzleFlash
        writeShort (edict^.eIndex)
        writeByte (Constants.mzShotgun .|. isSilenced)
        multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1
        playerNoise edictRef start Constants.pNoiseWeapon

        when (dmFlagsValue .&. Constants.dfInfiniteAmmo == 0) $
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (client^.gcAmmoIndex) -= 1

        return True

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
    return True

weaponRailgunFire :: EntThink
weaponRailgunFire =
  GenericEntThink "weapon_railgun_fire" $ \edictRef@(EdictReference edictIdx) -> do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar
    isQuad <- use $ gameBaseGlobals.gbIsQuad
    isSilenced <- use $ gameBaseGlobals.gbIsSilenced

    let (damage, kick) = if deathmatchValue /= 0 -- normal damage is too extreme in dm
                           then if isQuad
                                  then (100 * 4, 200 * 4)
                                  else (100, 200)
                           else if isQuad
                                  then (150 * 4, 250 * 4)
                                  else (150, 250)

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just client <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    let (Just forward, Just right, _) = Math3D.angleVectors (client^.gcVAngle) True True False
        kickOrigin = fmap (* (-3)) forward
        offset = V3 0 7 (fromIntegral (edict^.eViewHeight) - 8)

    zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
      gcKickOrigin .= kickOrigin
      gcKickAngles._x .= (-3)

    let start = projectSource client (edict^.eEntityState.esOrigin) offset forward right

    GameWeapon.fireRail edictRef start forward damage kick

    -- send muzzle flash
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast

    writeByte Constants.svcMuzzleFlash
    writeShort (edict^.eIndex)
    writeByte (Constants.mzRailgun .|. isSilenced)
    multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1
    playerNoise edictRef start Constants.pNoiseWeapon

    when (dmFlagsValue .&. Constants.dfInfiniteAmmo == 0) $
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (client^.gcAmmoIndex) -= 1

    return True

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

      if (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
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
    when ((edict^.eHealth) < 1) $ do
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
weaponGeneric edictRef@(EdictReference edictIdx) frameActiveLast frameFireLast frameIdleLast frameDeactivateLast pauseFrames fireFrames fire = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let Just gClientRef@(GClientReference gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    let frameFireFirst = frameActiveLast + 1
        frameIdleFirst = frameFireLast + 1
        frameDeactivateFirst = frameIdleLast + 1

    if | (edict^.eDeadFlag) /= 0 || (edict^.eEntityState.esModelIndex) /= 255 ->
           return () -- VWep animations screw up corpses

       | (gClient^.gcWeaponState) == Constants.weaponDropping -> do
           if | (gClient^.gcPlayerState.psGunFrame) == frameDeactivateLast ->
                  changeWeapon edictRef

              | frameDeactivateLast - (gClient^.gcPlayerState.psGunFrame) == 4 -> do
                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimPriority .= Constants.animReverse

                  if (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
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
           if (gClient^.gcPlayerState.psGunFrame) == frameActiveLast
             then do
               zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
                 gcWeaponState .= Constants.weaponReady
                 gcPlayerState.psGunFrame .= frameIdleFirst
             else do
               gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

       | isJust (gClient^.gcNewWeapon) && (gClient^.gcWeaponState) /= Constants.weaponFiring -> do
           zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
             gcWeaponState .= Constants.weaponDropping
             gcPlayerState.psGunFrame .= frameDeactivateFirst

           when (frameDeactivateLast - frameDeactivateFirst < 4) $ do
             gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimPriority .= Constants.animReverse

             if (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
               then do
                 gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.frameCRPain4 + 1
                 gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameCRPain1
               else do
                 gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.framePain304 + 1
                 gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.framePain301

       | otherwise -> do
           done <- if (gClient^.gcWeaponState) == Constants.weaponReady
                     then do
                       if ((gClient^.gcLatchedButtons) .|. (gClient^.gcButtons)) .&. Constants.buttonAttack /= 0
                         then do
                           gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcLatchedButtons %= (.&. (complement Constants.buttonAttack))
                           let Just (GItemReference weaponIdx) = gClient^.gcPers.cpWeapon
                           Just weapon <- preuse $ gameBaseGlobals.gbItemList.ix weaponIdx

                           if (gClient^.gcAmmoIndex) == 0 || ((gClient^.gcPers.cpInventory) UV.! (gClient^.gcAmmoIndex)) >= (weapon^.giQuantity)
                             then do
                               zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
                                 gcPlayerState.psGunFrame .= frameFireFirst
                                 gcWeaponState .= Constants.weaponFiring
                                 -- start the animation
                                 gcAnimPriority .= Constants.animAttack

                               if (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
                                 then do
                                   gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.frameCRAttack1 - 1
                                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameCRAttack9
                                 else do
                                   gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= MPlayer.frameAttack1 - 1
                                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameAttack8

                               return False

                             else do
                               levelTime <- use $ gameBaseGlobals.gbLevel.llTime
                               when (levelTime >= (edict^.ePainDebounceTime)) $ do
                                 soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
                                 sound <- use $ gameBaseGlobals.gbGameImport.giSound
                                 soundIdx <- soundIndex (Just "weapons/noammo.wav")
                                 sound (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
                                 gameBaseGlobals.gbGEdicts.ix edictIdx.ePainDebounceTime .= levelTime + 1

                               noAmmoWeaponChange edictRef
                               return False

                         else do
                           if (gClient^.gcPlayerState.psGunFrame) == frameIdleLast
                             then do
                               gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame .= frameIdleFirst
                               return True
                             else do
                               -- TODO: do we need this?
                               -- if (pause_frames != null) {
                               done <- checkPauseFrames gClient pauseFrames 0
                               if done
                                 then return True
                                 else do
                                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1
                                   return True
                     else
                       return False

           unless done $ do
             when ((gClient^.gcWeaponState) == Constants.weaponFiring) $ do
               idx <- checkFireFrames gClientRef fireFrames 0
               when (fireFrames UV.! idx == 0) $
                 gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

               preuse (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame) >>= \(Just gunFrame) ->
                 when (gunFrame == frameIdleFirst + 1) $
                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcWeaponState .= Constants.weaponReady

  where checkFireFrames :: GClientReference -> UV.Vector Int -> Int -> Quake Int
        checkFireFrames gClientRef@(GClientReference gClientIdx) fireFrames idx
          | fireFrames UV.! idx == 0 = return idx
          | otherwise = do
              Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
              if (gClient^.gcPlayerState.psGunFrame) == fireFrames UV.! idx
                then do
                  frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum
                  when (truncate (gClient^.gcQuadFrameNum) > frameNum) $ do
                    soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
                    sound <- use $ gameBaseGlobals.gbGameImport.giSound
                    soundIdx <- soundIndex (Just "items/damage3.wav")
                    sound (Just edictRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0

                  think fire edictRef
                  return idx
                else
                  checkFireFrames gClientRef fireFrames (idx + 1)

        checkPauseFrames :: GClientT -> UV.Vector Int -> Int -> Quake Bool
        checkPauseFrames gClient pauseFrames idx
          | pauseFrames UV.! idx == 0 = return False
          | otherwise = do
              if (gClient^.gcPlayerState.psGunFrame) == pauseFrames UV.! idx
                then do
                  r <- Lib.rand
                  if r .&. 15 /= 0
                    then return True
                    else checkPauseFrames gClient pauseFrames (idx + 1)
                else
                  checkPauseFrames gClient pauseFrames (idx + 1)


weaponGrenadeFire :: EdictReference -> Bool -> Quake ()
weaponGrenadeFire _ _ = do
    io (putStrLn "PlayerWeapon.weaponGrenadeFire") >> undefined -- TODO

blasterFire :: EdictReference -> V3 Float -> Int -> Bool -> Int -> Quake ()
blasterFire edictRef@(EdictReference edictIdx) gOffset dmg hyper effect = do
    isQuad <- use $ gameBaseGlobals.gbIsQuad
    isSilenced <- use $ gameBaseGlobals.gbIsSilenced

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    let damage = if isQuad then dmg * 4 else dmg
        (Just forward, Just right, _) = Math3D.angleVectors (gClient^.gcVAngle) True True False
        offset = (V3 24 8 (fromIntegral (edict^.eViewHeight) - 8)) + gOffset
        start = projectSource gClient (edict^.eEntityState.esOrigin) offset forward right
        V3 _ b c = fmap (* (-2)) forward

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcKickOrigin .= V3 (-1) b c

    GameWeapon.fireBlaster edictRef start forward damage 1000 effect hyper

    -- send muzzle flash
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast

    writeByte Constants.svcMuzzleFlash
    writeShort edictIdx
    if hyper
      then writeByte (Constants.mzHyperblaster .|. isSilenced)
      else writeByte (Constants.mzBlaster .|. isSilenced)
    multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

    playerNoise edictRef start Constants.pNoiseWeapon

{-
- =============== 
- PlayerNoise
- 
- Each player can have two noise objects associated with it: a personal
- noise (jumping, pain, weapon firing), and a weapon target noise (bullet
- wall impacts)
- 
- Monsters that don't directly see the player can move to a noise in hopes
- of seeing the player from there. 
- ===============
-}
playerNoise :: EdictReference -> V3 Float -> Int -> Quake ()
playerNoise whoRef@(EdictReference whoIdx) noiseLocation noiseType = do
    Just who <- preuse $ gameBaseGlobals.gbGEdicts.ix whoIdx
    let Just (GClientReference gClientIdx) = who^.eClient
    Just client <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if | noiseType == Constants.pNoiseWeapon && (client^.gcSilencerShots) > 0 ->
           gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcSilencerShots -= 1

       | deathmatchValue /= 0 || (who^.eFlags) .&. Constants.flNoTarget /= 0 ->
           return ()

       | otherwise -> do
           when (isNothing (who^.eMyNoise)) $ do
             noiseRef@(EdictReference noiseIdx) <- GameUtil.spawn

             zoom (gameBaseGlobals.gbGEdicts.ix noiseIdx) $ do
               eClassName .= "player_noise"
               eMins .= V3 (-8) (-8) (-8)
               eMaxs .= V3 8 8 8
               eOwner .= Just whoRef
               eSvFlags .= Constants.svfNoClient

             noiseRef'@(EdictReference noiseIdx') <- GameUtil.spawn

             zoom (gameBaseGlobals.gbGEdicts.ix noiseIdx') $ do
               eClassName .= "player_noise"
               eMins .= V3 (-8) (-8) (-8)
               eMaxs .= V3 8 8 8
               eOwner .= Just whoRef
               eSvFlags .= Constants.svfNoClient

             zoom (gameBaseGlobals.gbGEdicts.ix whoIdx) $ do
               eMyNoise .= Just noiseRef
               eMyNoise2 .= Just noiseRef'

           Just who' <- preuse $ gameBaseGlobals.gbGEdicts.ix whoIdx

           Just noiseRef@(EdictReference noiseIdx) <-
             if noiseType == Constants.pNoiseSelf || noiseType == Constants.pNoiseWeapon
               then do
                 frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

                 zoom (gameBaseGlobals.gbLevel) $ do
                   llSoundEntity .= (who'^.eMyNoise)
                   llSoundEntityFrameNum .= frameNum

                 return (who'^.eMyNoise)
               else do
                 frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

                 zoom (gameBaseGlobals.gbLevel) $ do
                   llSound2Entity .= (who'^.eMyNoise2)
                   llSound2EntityFrameNum .= frameNum

                 return (who'^.eMyNoise2)

           Just noise <- preuse $ gameBaseGlobals.gbGEdicts.ix noiseIdx
           levelTime <- use $ gameBaseGlobals.gbLevel.llTime

           zoom (gameBaseGlobals.gbGEdicts.ix noiseIdx) $ do
             eEntityState.esOrigin .= noiseLocation
             eAbsMin .= (noiseLocation - (noise^.eMaxs))
             eAbsMax .= (noiseLocation + (noise^.eMaxs))
             eTeleportTime .= levelTime

           linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
           linkEntity noiseRef

noAmmoWeaponChange :: EdictReference -> Quake ()
noAmmoWeaponChange _ = do
    io (putStrLn "PlayerWeapon.noAmmoWeaponChange") >> undefined -- TODO

projectSource :: GClientT -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float
projectSource client point distance forward right =
    let V3 a b c = distance
        distance' = if | (client^.gcPers.cpHand) == Constants.leftHanded -> V3 a (negate b) c
                       | (client^.gcPers.cpHand) == Constants.centerHanded -> V3 a 0 c
                       | otherwise -> distance
    in Math3D.projectSource point distance' forward right
