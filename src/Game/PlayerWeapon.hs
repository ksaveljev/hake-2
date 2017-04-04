{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Game.PlayerWeapon where

import Control.Lens (use, preuse, ix, (.=), (^.), zoom, (+=), (-=), (%=), (&), (.~), (%~), (-~), (+~))
import Control.Monad (when, liftM, void, unless)
import Data.Bits ((.&.), (.|.), shiftL, complement)
import Data.Maybe (isJust, isNothing, fromJust)
import Linear (V3(..), _x, _y)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

import Game.PMoveT
import Game.EntityStateT
import Game.EdictT
import Game.GClientT
import Game.PlayerStateT
import Game.MonsterInfoT
import Game.ClientPersistantT
import Types
import Game.PMoveStateT
import QuakeRef
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameItems as GameItems
import qualified Game.GameUtil as GameUtil
import {-# SOURCE #-} qualified Game.GameWeapon as GameWeapon
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
  GenericItemUse "Use_Weapon" $ \edictRef gItemRef@(GItemReference gItemIdx) -> do
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
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
                 cprintf (Just edictRef) Constants.printHigh ("No " `B.append` (fromJust $ ammoItem^.giPickupName) `B.append` " for " `B.append` (fromJust $ item^.giPickupName) `B.append` ".\n")

             | quantity < (item^.giQuantity) -> do
                 cprintf (Just edictRef) Constants.printHigh ("Not enough " `B.append` (fromJust $ ammoItem^.giPickupName) `B.append` " for " `B.append` (fromJust $ item^.giPickupName) `B.append` ".\n")

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
  GenericEntThink "Weapon_Blaster_Fire" $ \edictRef -> do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    let damage = if deathmatchValue /= 0 then 15 else 10
    v3o <- use $ globals.gVec3Origin

    blasterFire edictRef v3o damage False Constants.efBlaster

    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

    return True

pickupWeapon :: EntInteract
pickupWeapon =
  GenericEntInteract "Pickup_Weapon" $ \edictRef otherRef -> do
    edict <- readRef edictRef
    other <- readRef otherRef

    let Just (GItemReference itemIdx) = edict^.eItem
    Just item <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx
    let index = item^.giIndex

    dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar
    coopValue <- liftM (^.cvValue) coopCVar
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    let Just (Ref otherClientIdx) = other^.eClient
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
                then modifyRef edictRef (\v -> v & eFlags %~ (.|. Constants.flRespawn))
                else GameItems.setRespawn edictRef 30

            when (coopValue /= 0) $
              modifyRef edictRef (\v -> v & eFlags %~ (.|. Constants.flRespawn))

        edict' <- readRef edictRef
        Just otherClient' <- preuse $ gameBaseGlobals.gbGame.glClients.ix otherClientIdx

        blasterIdx <- GameItems.findItem "blaster"

        when ((otherClient'^.gcPers.cpWeapon) /= (edict'^.eItem) && (otherClient'^.gcPers.cpInventory) UV.! index == 1 && (deathmatchValue == 0 || (otherClient'^.gcPers.cpWeapon) == blasterIdx)) $
          gameBaseGlobals.gbGame.glClients.ix otherClientIdx.gcNewWeapon .= (edict'^.eItem)

        return True

dropWeapon :: ItemDrop
dropWeapon =
  GenericItemDrop "Drop_Weapon" $ \edictRef itemRef@(GItemReference gItemIdx) -> do
    dmFlagsValue :: Int <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    when (dmFlagsValue .&. Constants.dfWeaponsStay == 0) $ do
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      edict <- readRef edictRef
      let Just (Ref gClientIdx) = edict^.eClient
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

      let index = gItem^.giIndex

      if ((Just itemRef) == (gClient^.gcPers.cpWeapon) || (Just itemRef) == (gClient^.gcNewWeapon)) && ((gClient^.gcPers.cpInventory) UV.! index) == 1
        then do
          cprintf <- use $ gameBaseGlobals.gbGameImport.giCprintf
          cprintf (Just edictRef) Constants.printHigh "Can't drop current weapon\n"
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
  GenericEntThink "weapon_shotgun_fire" $ \edictRef -> do
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
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
  GenericEntThink "weapon_supershotgun_fire" $ \edictRef -> do
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    let (Just forward, Just right, _) = Math3D.angleVectors (gClient^.gcVAngle) True True False

    zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
      gcKickOrigin .= fmap (* (-2)) forward
      gcKickAngles._x -= 2

    isQuad <- use $ gameBaseGlobals.gbIsQuad
    isSilenced <- use $ gameBaseGlobals.gbIsSilenced

    let offset = V3 0 8 (fromIntegral (edict^.eViewHeight) - 8)
        start = projectSource gClient (edict^.eEntityState.esOrigin) offset forward right
        (damage, kick) = if isQuad
                           then (6 * 4, 12 * 4)
                           else (6, 12)
        v = (gClient^.gcVAngle) & _y -~ 5 -- IMPROVE: use Constants.yaw instead of using _y directly
        (Just forward', _, _) = Math3D.angleVectors v True False False

    GameWeapon.fireShotgun edictRef start forward' damage kick Constants.defaultShotgunHspread Constants.defaultShotgunVspread (Constants.defaultSshotgunCount `div` 2) Constants.modSshotgun

    let v' = (gClient^.gcVAngle) & _y +~ 5 -- IMPROVE: use Constants.yaw instead of using _y directly
        (Just forward'', _, _) = Math3D.angleVectors v' True False False

    GameWeapon.fireShotgun edictRef start forward'' damage kick Constants.defaultShotgunHspread Constants.defaultShotgunVspread (Constants.defaultSshotgunCount `div` 2) Constants.modSshotgun

    -- send muzzle flash
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast

    writeByte Constants.svcMuzzleFlash
    writeShort (edict^.eIndex)
    writeByte (Constants.mzSShotgun .|. isSilenced)
    multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

    playerNoise edictRef start Constants.pNoiseWeapon

    dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar
    when (dmFlagsValue .&. Constants.dfInfiniteAmmo == 0) $
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gClient^.gcAmmoIndex) -= 2

    return True

weaponMachinegun :: EntThink
weaponMachinegun = 
  GenericEntThink "Weapon_Machinegun" $ \edictRef -> do
    let pauseFrames = UV.fromList [23, 45, 0]
        fireFrames = UV.fromList [4, 5, 0]

    weaponGeneric edictRef 3 5 45 49 pauseFrames fireFrames machinegunFire
    return True

machinegunFire :: EntThink
machinegunFire =
  GenericEntThink "Machinegun_Fire" $ \edictRef -> do
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound
        writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast

    if (gClient^.gcButtons) .&. Constants.buttonAttack == 0
      then do
        zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
          gcMachinegunShots .= 0
          gcPlayerState.psGunFrame += 1

        return True

      else do
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame .= if (gClient^.gcPlayerState.psGunFrame) == 5
                                                                                     then 4
                                                                                     else 5

        if (gClient^.gcPers.cpInventory) UV.! (gClient^.gcAmmoIndex) < 1
          then do
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame .= 6
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime

            when (levelTime >= (edict^.ePainDebounceTime)) $ do
              soundIdx <- soundIndex (Just "weapons/noammo.wav")
              sound (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
              modifyRef edictRef (\v -> v & ePainDebounceTime .~ levelTime + 1)

            noAmmoWeaponChange edictRef

            return True

          else do
            isQuad <- use $ gameBaseGlobals.gbIsQuad
            isSilenced <- use $ gameBaseGlobals.gbIsSilenced

            o1 <- Lib.crandom
            o2 <- Lib.crandom
            o3 <- Lib.crandom
            a2 <- Lib.crandom
            a3 <- Lib.crandom

            let (damage, kick) = if isQuad
                                   then (8 * 4, 2 * 4)
                                   else (8, 2)
                kickOrigin = fmap (* 0.35) (V3 o1 o2 o3)
                kickAngles = V3 ((-1.5) * fromIntegral (gClient^.gcMachinegunShots)) (a2 * 0.7) (a3 * 0.7)

            zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
              gcKickOrigin .= kickOrigin
              gcKickAngles .= kickAngles

            -- raise the gun as it is firing
            deathmatchValue <- liftM (^.cvValue) deathmatchCVar

            when (deathmatchValue == 0) $ do
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcMachinegunShots %= (\v -> if v + 1 > 9 then 9 else v + 1)

            -- get start / end positions
            let angles = (gClient^.gcVAngle) + kickAngles
                (Just forward, Just right, _) = Math3D.angleVectors angles True True False
                offset = V3 0 8 (fromIntegral (edict^.eViewHeight) - 8)
                start = projectSource gClient (edict^.eEntityState.esOrigin) offset forward right

            GameWeapon.fireBullet edictRef start forward damage kick Constants.defaultBulletHspread Constants.defaultBulletVspread Constants.modMachinegun

            writeByte Constants.svcMuzzleFlash
            writeShort (edict^.eIndex)
            writeByte (Constants.mzMachinegun .|. isSilenced)
            multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

            playerNoise edictRef start Constants.pNoiseWeapon

            dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar

            when (dmFlagsValue .&. Constants.dfInfiniteAmmo == 0) $
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gClient^.gcAmmoIndex) -= 1

            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimPriority .= Constants.animAttack

            r <- Lib.randomF
            let (frame, animEnd) = if (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
                                     then (MPlayer.frameCRAttack1 - truncate (r + 0.25), MPlayer.frameCRAttack9)
                                     else (MPlayer.frameAttack1 - truncate (r + 0.25), MPlayer.frameAttack8)

            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ frame)
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= animEnd

            return True

weaponChaingun :: EntThink
weaponChaingun = 
  GenericEntThink "Weapon_Chaingun" $ \edictRef -> do
    let pauseFrames = UV.fromList [38, 43, 51, 61, 0]
        fireFrames = UV.fromList [5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 0]

    weaponGeneric edictRef 4 31 61 64 pauseFrames fireFrames chaingunFire
    return True

chaingunFire :: EntThink
chaingunFire =
  GenericEntThink "Chaingun_Fire" $ \edictRef -> do
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    gameImport <- use $ gameBaseGlobals.gbGameImport
    isQuad <- use $ gameBaseGlobals.gbIsQuad
    isSilenced <- use $ gameBaseGlobals.gbIsSilenced

    let soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound
        writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast

    let (damage, kick) = let dmg = if deathmatchValue /= 0 then 6 else 8
                         in if isQuad
                              then (dmg * 4, 2 * 4)
                              else (dmg, 2)

    when ((gClient^.gcPlayerState.psGunFrame) == 5) $ do
      soundIdx <- soundIndex (Just "weapons/chngnu1a.wav")
      sound (Just edictRef) Constants.chanAuto soundIdx 1 Constants.attnIdle 0

    if (gClient^.gcPlayerState.psGunFrame) == 14 && (gClient^.gcButtons) .&. Constants.buttonAttack == 0
      then do
        zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
          gcPlayerState.psGunFrame .= 32
          gcWeaponSound .= 0

      else do
        let gunFrame = if (gClient^.gcPlayerState.psGunFrame) == 21 && (gClient^.gcButtons) .&. Constants.buttonAttack /= 0 && ((gClient^.gcPers.cpInventory) UV.! (gClient^.gcAmmoIndex)) /= 0
                         then 15
                         else (gClient^.gcPlayerState.psGunFrame) + 1

        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame .= gunFrame

        if gunFrame == 22
          then do
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcWeaponSound .= 0
            soundIdx <- soundIndex (Just "weapons/chngnd1a.wav")
            sound (Just edictRef) Constants.chanAuto soundIdx 1 Constants.attnIdle 0

          else do
            soundIdx <- soundIndex (Just "weapons/chngnl1a.wav")
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcWeaponSound .= soundIdx

        let (frame, animEnd) = if (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
                                 then (MPlayer.frameCRAttack1 - (gunFrame .&. 1), MPlayer.frameCRAttack9)
                                 else (MPlayer.frameAttack1 - (gunFrame .&. 1), MPlayer.frameAttack8)

        modifyRef edictRef (\v -> v & eEntityState.esFrame .~ frame)

        zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
          gcAnimPriority .= Constants.animAttack
          gcAnimEnd .= animEnd

        let shots = if | gunFrame <= 9 -> 1
                       | gunFrame <= 14 -> if (gClient^.gcButtons) .&. Constants.buttonAttack /= 0 then 2 else 1
                       | otherwise -> 3
            ammo = (gClient^.gcPers.cpInventory) UV.! (gClient^.gcAmmoIndex)
            shots' = if ammo < shots then ammo else shots

        if shots' == 0
          then do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime

            when (levelTime >= (edict^.ePainDebounceTime)) $ do
              soundIdx <- soundIndex (Just "weapons/noammo.wav")
              sound (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
              modifyRef edictRef (\v -> v & ePainDebounceTime .~ levelTime + 1)

            noAmmoWeaponChange edictRef

          else do
            o1 <- Lib.crandom
            o2 <- Lib.crandom
            o3 <- Lib.crandom
            a1 <- Lib.crandom
            a2 <- Lib.crandom
            a3 <- Lib.crandom

            let kickOrigin = fmap (* 0.35) (V3 o1 o2 o3)
                kickAngles = fmap (* 0.7) (V3 a1 a2 a3)

            start <- doShots edictRef edict gClient damage kick (V3 0 0 0) 0 shots'

            -- send muzzle flash
            writeByte Constants.svcMuzzleFlash
            writeShort (edict^.eIndex)
            writeByte ((Constants.mzChaingun1 + shots' - 1) .|. isSilenced)
            multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

            playerNoise edictRef start Constants.pNoiseWeapon

            dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar

            when (dmFlagsValue .&. Constants.dfInfiniteAmmo == 0) $
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gClient^.gcAmmoIndex) -= shots'

    return True

  where doShots :: Ref EdictT -> EdictT -> GClientT -> Int -> Int -> V3 Float -> Int -> Int -> Quake (V3 Float)
        doShots edictRef edict gClient damage kick start idx maxIdx
          | idx >= maxIdx = return start
          | otherwise = do
              r <- Lib.crandom
              u <- Lib.crandom

              let (Just forward, Just right, Just up) = Math3D.angleVectors (gClient^.gcVAngle) True True True
                  offset = V3 0 (7 + r * 4) (u * 4 + fromIntegral (edict^.eViewHeight) - 8)
                  start' = projectSource gClient (edict^.eEntityState.esOrigin) offset forward right

              GameWeapon.fireBullet edictRef start' forward damage kick Constants.defaultBulletHspread Constants.defaultBulletVspread Constants.modChaingun

              doShots edictRef edict gClient damage kick start' (idx + 1) maxIdx


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
  GenericEntThink "weapon_grenadelauncher_fire" $ \edictRef -> do
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    isQuad <- use $ gameBaseGlobals.gbIsQuad
    isSilenced <- use $ gameBaseGlobals.gbIsSilenced
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast

    let radius = 120 + 40
        damage = if isQuad then 120 * 4 else 120
        offset = V3 8 8 (fromIntegral (edict^.eViewHeight) - 8)
        (Just forward, Just right, _) = Math3D.angleVectors (gClient^.gcVAngle) True True False
        start = projectSource gClient (edict^.eEntityState.esOrigin) offset forward right

    zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
      gcKickOrigin .= fmap (* (-2)) forward
      gcKickAngles._x .= -1

    GameWeapon.fireGrenade edictRef start forward damage 600 2.5 radius

    writeByte Constants.svcMuzzleFlash
    writeShort (edict^.eIndex)
    writeByte (Constants.mzGrenade .|. isSilenced)
    multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

    playerNoise edictRef start Constants.pNoiseWeapon

    dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    when (dmFlagsValue .&. Constants.dfInfiniteAmmo == 0) $
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gClient^.gcAmmoIndex) -= 1

    return True

weaponRocketLauncher :: EntThink
weaponRocketLauncher = 
  GenericEntThink "Weapon_RocketLauncher" $ \edictRef -> do
    let pauseFrames = UV.fromList [25, 33, 42, 50, 0]
        fireFrames = UV.fromList [5, 0]

    weaponGeneric edictRef 4 12 50 54 pauseFrames fireFrames weaponRocketLauncherFire
    return True

weaponRocketLauncherFire :: EntThink
weaponRocketLauncherFire =
  GenericEntThink "Weapon_RocketLauncher_Fire" $ \edictRef -> do
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    r <- Lib.randomF
    isQuad <- use $ gameBaseGlobals.gbIsQuad
    isSilenced <- use $ gameBaseGlobals.gbIsSilenced
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast
        (damage, radiusDamage) = let d = 100 + truncate (r * 20.0)
                                     rd = 120
                                 in if isQuad
                                      then (d * 4, rd * 4)
                                      else (d, rd)
        damageRadius = 120
        (Just forward, Just right, _) = Math3D.angleVectors (gClient^.gcVAngle) True True False
        offset = V3 8 8 (fromIntegral (edict^.eViewHeight) - 8)
        start = projectSource gClient (edict^.eEntityState.esOrigin) offset forward right

    zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
      gcKickOrigin .= fmap (* (-2)) forward
      gcKickAngles._x .= -1

    GameWeapon.fireRocket edictRef start forward damage 650 damageRadius radiusDamage

    -- send muzzle flash
    writeByte Constants.svcMuzzleFlash
    writeShort (edict^.eIndex)
    writeByte (Constants.mzRocket .|. isSilenced)
    multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

    playerNoise edictRef start Constants.pNoiseWeapon

    dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    when (dmFlagsValue .&. Constants.dfInfiniteAmmo == 0) $
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gClient^.gcAmmoIndex) -= 1

    return True

weaponHyperBlaster :: EntThink
weaponHyperBlaster = 
  GenericEntThink "Weapon_HyperBlaster" $ \edictRef -> do
    let pauseFrames = UV.fromList [0]
        fireFrames = UV.fromList [6, 7, 8, 9, 10, 11, 0]

    weaponGeneric edictRef 5 20 49 53 pauseFrames fireFrames weaponHyperBlasterFire
    return True

weaponHyperBlasterFire :: EntThink
weaponHyperBlasterFire =
  GenericEntThink "Weapon_HyperBlaster_Fire" $ \edictRef -> do
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex

    if (gClient^.gcButtons) .&. Constants.buttonAttack == 0
      then
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

      else do
        if (gClient^.gcPers.cpInventory) UV.! (gClient^.gcAmmoIndex) == 0
          then do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime

            when (levelTime >= (edict^.ePainDebounceTime)) $ do
              soundIdx <- soundIndex (Just "weapons/noammo.wav")
              sound (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
              modifyRef edictRef (\v -> v & ePainDebounceTime .~ levelTime + 1)

            noAmmoWeaponChange edictRef

          else do
            deathmatchValue <- liftM (^.cvValue) deathmatchCVar

            let rotation = (fromIntegral (gClient^.gcPlayerState.psGunFrame) - 5) * 2 * pi / 6
                offset = V3 ((-4) * sin rotation) 0 (4 * cos rotation)
                effect = if (gClient^.gcPlayerState.psGunFrame) == 6 || (gClient^.gcPlayerState.psGunFrame) == 9
                           then Constants.efHyperblaster
                           else 0
                damage = if deathmatchValue /= 0 then 15 else 20

            blasterFire edictRef offset damage True effect

            dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar

            when (dmFlagsValue .&. Constants.dfInfiniteAmmo == 0) $
              gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gClient^.gcAmmoIndex) -= 1

            let (frame, animEnd) = if (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
                                     then (MPlayer.frameCRAttack1 - 1, MPlayer.frameCRAttack9)
                                     else (MPlayer.frameAttack1 - 1, MPlayer.frameAttack8)

            modifyRef edictRef (\v -> v & eEntityState.esFrame .~ frame)

            zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
              gcAnimPriority .= Constants.animAttack
              gcAnimEnd .= animEnd

        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

        Just gClient' <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

        when ((gClient'^.gcPlayerState.psGunFrame) == 12 && (gClient'^.gcPers.cpInventory) UV.! (gClient'^.gcAmmoIndex) /= 0) $
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame .= 6

    Just gClient' <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    when ((gClient'^.gcPlayerState.psGunFrame) == 12) $ do
      soundIdx <- soundIndex (Just "weapons/hyprbd1a.wav")
      sound (Just edictRef) Constants.chanAuto soundIdx 1 Constants.attnNorm 0
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcWeaponSound .= 0

    return True

weaponRailgun :: EntThink
weaponRailgun = 
  GenericEntThink "Weapon_Railgun" $ \edictRef -> do
    let pauseFrames = UV.fromList [56, 0]
        fireFrames = UV.fromList [4, 0]

    weaponGeneric edictRef 3 18 56 61 pauseFrames fireFrames weaponRailgunFire
    return True

weaponRailgunFire :: EntThink
weaponRailgunFire =
  GenericEntThink "weapon_railgun_fire" $ \edictRef -> do
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

    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
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
  GenericEntThink "weapon_bfg_fire" $ \edictRef -> do
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    isQuad <- use $ gameBaseGlobals.gbIsQuad
    isSilenced <- use $ gameBaseGlobals.gbIsSilenced
    gameImport <- use $ gameBaseGlobals.gbGameImport
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    let writeByte = gameImport^.giWriteByte
        writeShort = gameImport^.giWriteShort
        multicast = gameImport^.giMulticast

    if | (gClient^.gcPlayerState.psGunFrame) == 9 -> do
           -- send muzzle flash
           writeByte Constants.svcMuzzleFlash
           writeShort (edict^.eIndex)
           writeByte (Constants.mzBFG .|. isSilenced)
           multicast (edict^.eEntityState.esOrigin) Constants.multicastPvs

           gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

           playerNoise edictRef (V3 0 0 0) Constants.pNoiseWeapon

         -- cells can go down during windup (from power armor hits), so
         -- check again and abort firing if we don't have enough now
       | (gClient^.gcPers.cpInventory) UV.! (gClient^.gcAmmoIndex) < 50 ->
           gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

       | otherwise -> do
           let damageRadius = 1000
               damage = let d = if deathmatchValue /= 0 then 200 else 500
                        in if isQuad then d * 4 else d
               (Just forward, Just right, _) = Math3D.angleVectors (gClient^.gcVAngle) True True False

           r <- Lib.crandom
           levelTime <- use $ gameBaseGlobals.gbLevel.llTime

           zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
             gcKickOrigin .= fmap (* (-2)) forward
             -- make a big pitch kick with an inverse fall
             gcVDmgPitch .= -40
             gcVDmgRoll .= r * 8
             gcVDmgTime .= levelTime + Constants.damageTime

           Just gClient' <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

           let offset = V3 8 8 (fromIntegral (edict^.eViewHeight) - 8)
               start = projectSource gClient' (edict^.eEntityState.esOrigin) offset forward right

           GameWeapon.fireBFG edictRef start forward damage 400 damageRadius

           gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psGunFrame += 1

           playerNoise edictRef start Constants.pNoiseWeapon

           dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar

           when (dmFlagsValue .&. Constants.dfInfiniteAmmo == 0) $
             gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gClient^.gcAmmoIndex) -= 50

    return True

changeWeapon :: Ref EdictT -> Quake ()
changeWeapon edictRef = do
    edict <- readRef edictRef
    let Just gClientRef@(Ref gClientIdx) = edict^.eClient

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
          modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameCRPain1)
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameCRPain4
        else do
          modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.framePain301)
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.framePain304

  where checkGrenadeTime :: Ref GClientT -> Quake ()
        checkGrenadeTime (Ref gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          when (gClient^.gcGrenadeTime /= 0) $ do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcGrenadeTime .= levelTime
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcWeaponSound .= 0
            weaponGrenadeFire edictRef False
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcGrenadeTime .= 0

        setVisibleModel :: Ref GClientT -> Quake ()
        setVisibleModel (Ref gClientIdx) = do
          edict <- readRef edictRef

          when ((edict^.eEntityState.esModelIndex) == 255) $ do
            Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

            i <- case gClient^.gcPers.cpWeapon of
                   Nothing -> return 0
                   Just (GItemReference weaponIdx) -> do
                     Just weaponModel <- preuse $ gameBaseGlobals.gbItemList.ix weaponIdx.giWeaponModel
                     return $ (weaponModel .&. 0xFF) `shiftL` 8

            modifyRef edictRef (\v -> v & eEntityState.esSkinNum .~ ((edict^.eIndex) - 1) .|. i)

        setAmmoAndGunIndex :: Ref GClientT -> Quake Bool
        setAmmoAndGunIndex (Ref gClientIdx) = do
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
thinkWeapon :: Ref EdictT -> Quake ()
thinkWeapon edictRef = do
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient

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

weaponGeneric :: Ref EdictT -> Int -> Int -> Int -> Int -> UV.Vector Int -> UV.Vector Int -> EntThink -> Quake ()
weaponGeneric edictRef frameActiveLast frameFireLast frameIdleLast frameDeactivateLast pauseFrames fireFrames fire = do
    edict <- readRef edictRef
    let Just gClientRef@(Ref gClientIdx) = edict^.eClient
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
                      modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameCRPain4 + 1)
                      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameCRPain1
                    else do
                      modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.framePain304 + 1)
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
                 modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameCRPain4 + 1)
                 gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameCRPain1
               else do
                 modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.framePain304 + 1)
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
                                   modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameCRAttack1 - 1)
                                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameCRAttack9
                                 else do
                                   modifyRef edictRef (\v -> v & eEntityState.esFrame .~ MPlayer.frameAttack1 - 1)
                                   gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcAnimEnd .= MPlayer.frameAttack8

                               return False

                             else do
                               levelTime <- use $ gameBaseGlobals.gbLevel.llTime
                               when (levelTime >= (edict^.ePainDebounceTime)) $ do
                                 soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
                                 sound <- use $ gameBaseGlobals.gbGameImport.giSound
                                 soundIdx <- soundIndex (Just "weapons/noammo.wav")
                                 sound (Just edictRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0
                                 modifyRef edictRef (\v -> v & ePainDebounceTime .~ levelTime + 1)

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

  where checkFireFrames :: Ref GClientT -> UV.Vector Int -> Int -> Quake Int
        checkFireFrames gClientRef@(Ref gClientIdx) fireFrames idx
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


weaponGrenadeFire :: Ref EdictT -> Bool -> Quake ()
weaponGrenadeFire edictRef held = do
    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    isQuad <- use $ gameBaseGlobals.gbIsQuad
    isSilenced <- use $ gameBaseGlobals.gbIsSilenced
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let radius = 125 + 40
        damage = if isQuad then 125 * 4 else 125
        offset = V3 8 8 (fromIntegral (edict^.eViewHeight) - 8)
        (Just forward, Just right, _) = Math3D.angleVectors (gClient^.gcVAngle) True True False
        start = projectSource gClient (edict^.eEntityState.esOrigin) offset forward right
        timer = (gClient^.gcGrenadeTime) - levelTime
        speed = Constants.grenadeMinSpeed + truncate ((Constants.grenadeTimer - timer) * (fromIntegral (Constants.grenadeMaxSpeed - Constants.grenadeMinSpeed) / Constants.grenadeTimer))

    GameWeapon.fireGrenade2 edictRef start forward damage speed timer radius held

    dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    when (dmFlagsValue .&. Constants.dfInfiniteAmmo == 0) $
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gClient^.gcAmmoIndex) -= 1

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcGrenadeTime .= levelTime + 1.0

    edict' <- readRef edictRef

    unless ((edict'^.eDeadFlag) /= 0 || (edict'^.eEntityState.esModelIndex) /= 255 || (edict'^.eHealth) <= 0) $ do
      let (priority, frame, animEnd) = if (gClient^.gcPlayerState.psPMoveState.pmsPMFlags) .&. pmfDucked /= 0
                                         then (Constants.animAttack, MPlayer.frameCRAttack1 - 1, MPlayer.frameCRAttack3)
                                         else (Constants.animReverse, MPlayer.frameWave08, MPlayer.frameWave01)

      modifyRef edictRef (\v -> v & eEntityState.esFrame .~ frame)

      zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx) $ do
        gcAnimPriority .= priority
        gcAnimEnd .= animEnd

blasterFire :: Ref EdictT -> V3 Float -> Int -> Bool -> Int -> Quake ()
blasterFire edictRef gOffset dmg hyper effect = do
    isQuad <- use $ gameBaseGlobals.gbIsQuad
    isSilenced <- use $ gameBaseGlobals.gbIsSilenced

    edict <- readRef edictRef
    let Just (Ref gClientIdx) = edict^.eClient
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
    writeShort (edict^.eIndex)
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
playerNoise :: Ref EdictT -> V3 Float -> Int -> Quake ()
playerNoise whoRef noiseLocation noiseType = do
    who <- readRef whoRef
    let Just (Ref gClientIdx) = who^.eClient
    Just client <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if | noiseType == Constants.pNoiseWeapon && (client^.gcSilencerShots) > 0 ->
           gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcSilencerShots -= 1

       | deathmatchValue /= 0 || (who^.eFlags) .&. Constants.flNoTarget /= 0 ->
           return ()

       | otherwise -> do
           when (isNothing (who^.eMyNoise)) $ do
             noiseRef <- GameUtil.spawn

             modifyRef noiseRef (\v -> v & eClassName .~ "player_noise"
                                            & eMins .~ V3 (-8) (-8) (-8)
                                            & eMaxs .~ V3 8 8 8
                                            & eOwner .~ Just whoRef
                                            & eSvFlags .~ Constants.svfNoClient)

             noiseRef' <- GameUtil.spawn

             modifyRef noiseRef' (\v -> v & eClassName .~ "player_noise"
                                             & eMins .~ V3 (-8) (-8) (-8)
                                             & eMaxs .~ V3 8 8 8
                                             & eOwner .~ Just whoRef
                                             & eSvFlags .~ Constants.svfNoClient)

             modifyRef whoRef (\v -> v & eMyNoise .~ Just noiseRef
                                          & eMyNoise2 .~ Just noiseRef')

           who' <- readRef whoRef

           Just noiseRef <-
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

           noise <- readRef noiseRef
           levelTime <- use $ gameBaseGlobals.gbLevel.llTime

           modifyRef noiseRef (\v -> v & eEntityState.esOrigin .~ noiseLocation
                                          & eAbsMin .~ (noiseLocation - (noise^.eMaxs))
                                          & eAbsMax .~ (noiseLocation + (noise^.eMaxs))
                                          & eTeleportTime .~ levelTime)

           linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
           linkEntity noiseRef

noAmmoWeaponChange :: Ref EdictT -> Quake ()
noAmmoWeaponChange edictRef = do
    edict <- readRef edictRef
    let Just gClientRef@(Ref gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    checkSlugsAndRailgun gClientRef gClient

  where checkSlugsAndRailgun :: Ref GClientT -> GClientT -> Quake ()
        checkSlugsAndRailgun gClientRef@(Ref gClientIdx) gClient = do
          Just (GItemReference slugsIdx) <- GameItems.findItem "slugs"
          Just railgunRef@(GItemReference railgunIdx) <- GameItems.findItem "railgun"
          Just slugs <- preuse $ gameBaseGlobals.gbItemList.ix slugsIdx
          Just railgun <- preuse $ gameBaseGlobals.gbItemList.ix railgunIdx
          let inventory = gClient^.gcPers.cpInventory

          if inventory UV.! (slugs^.giIndex) /= 0 && inventory UV.! (railgun^.giIndex) /= 0
            then gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcNewWeapon .= Just railgunRef
            else checkCellsAndHyperblaster gClientRef gClient

        checkCellsAndHyperblaster :: Ref GClientT -> GClientT -> Quake ()
        checkCellsAndHyperblaster gClientRef@(Ref gClientIdx) gClient = do
          Just (GItemReference cellsIdx) <- GameItems.findItem "cells"
          Just hyperblasterRef@(GItemReference hyperblasterIdx) <- GameItems.findItem "hyperblaster"
          Just cells <- preuse $ gameBaseGlobals.gbItemList.ix cellsIdx
          Just hyperblaster <- preuse $ gameBaseGlobals.gbItemList.ix hyperblasterIdx
          let inventory = gClient^.gcPers.cpInventory

          if inventory UV.! (cells^.giIndex) /= 0 && inventory UV.! (hyperblaster^.giIndex) /= 0
            then gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcNewWeapon .= Just hyperblasterRef
            else checkBulletsAndChaingun gClientRef gClient

        checkBulletsAndChaingun :: Ref GClientT -> GClientT -> Quake ()
        checkBulletsAndChaingun gClientRef@(Ref gClientIdx) gClient = do
          Just (GItemReference bulletsIdx) <- GameItems.findItem "bullets"
          Just chaingunRef@(GItemReference chaingunIdx) <- GameItems.findItem "chaingun"
          Just bullets <- preuse $ gameBaseGlobals.gbItemList.ix bulletsIdx
          Just chaingun <- preuse $ gameBaseGlobals.gbItemList.ix chaingunIdx
          let inventory = gClient^.gcPers.cpInventory

          if inventory UV.! (bullets^.giIndex) /= 0 && inventory UV.! (chaingun^.giIndex) /= 0
            then gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcNewWeapon .= Just chaingunRef
            else checkBulletsAndMachinegun gClientRef gClient

        checkBulletsAndMachinegun :: Ref GClientT -> GClientT -> Quake ()
        checkBulletsAndMachinegun gClientRef@(Ref gClientIdx) gClient = do
          Just (GItemReference bulletsIdx) <- GameItems.findItem "bullets"
          Just machinegunRef@(GItemReference machinegunIdx) <- GameItems.findItem "machinegun"
          Just bullets <- preuse $ gameBaseGlobals.gbItemList.ix bulletsIdx
          Just machinegun <- preuse $ gameBaseGlobals.gbItemList.ix machinegunIdx
          let inventory = gClient^.gcPers.cpInventory

          if inventory UV.! (bullets^.giIndex) /= 0 && inventory UV.! (machinegun^.giIndex) /= 0
            then gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcNewWeapon .= Just machinegunRef
            else checkShellsAndSuperShotgun gClientRef gClient

        checkShellsAndSuperShotgun :: Ref GClientT -> GClientT -> Quake ()
        checkShellsAndSuperShotgun gClientRef@(Ref gClientIdx) gClient = do
          Just (GItemReference shellsIdx) <- GameItems.findItem "shells"
          Just superShotgunRef@(GItemReference superShotgunIdx) <- GameItems.findItem "super shotgun"
          Just shells <- preuse $ gameBaseGlobals.gbItemList.ix shellsIdx
          Just superShotgun <- preuse $ gameBaseGlobals.gbItemList.ix superShotgunIdx
          let inventory = gClient^.gcPers.cpInventory

          if inventory UV.! (shells^.giIndex) > 1 && inventory UV.! (superShotgun^.giIndex) /= 0
            then gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcNewWeapon .= Just superShotgunRef
            else checkShellsAndShotgun gClientRef gClient

        checkShellsAndShotgun :: Ref GClientT -> GClientT -> Quake ()
        checkShellsAndShotgun gClientRef@(Ref gClientIdx) gClient = do
          Just (GItemReference shellsIdx) <- GameItems.findItem "shells"
          Just shotgunRef@(GItemReference shotgunIdx)      <- GameItems.findItem "shotgun"
          Just blasterRef <- GameItems.findItem "blaster"
          Just shells <- preuse $ gameBaseGlobals.gbItemList.ix shellsIdx
          Just shotgun <- preuse $ gameBaseGlobals.gbItemList.ix shotgunIdx
          let inventory = gClient^.gcPers.cpInventory

          if inventory UV.! (shells^.giIndex) /= 0 && inventory UV.! (shotgun^.giIndex) /= 0
            then gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcNewWeapon .= Just shotgunRef
            else gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcNewWeapon .= Just blasterRef

projectSource :: GClientT -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float
projectSource client point distance forward right =
    let V3 a b c = distance
        distance' = if | (client^.gcPers.cpHand) == Constants.leftHanded -> V3 a (negate b) c
                       | (client^.gcPers.cpHand) == Constants.centerHanded -> V3 a 0 c
                       | otherwise -> distance
    in Math3D.projectSource point distance' forward right
