{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Game.GameItems where

import Control.Lens ((.=), (^.), use, preuse, ix, (%=), (+=), (-=), zoom, (&), (.~), (%~), (+~))
import Control.Monad (when, void, liftM, unless)
import Data.Bits ((.&.), (.|.), shiftL, complement, shiftR)
import Data.Char (toLower)
import Data.Maybe (fromJust, isNothing, isJust)
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import {-# SOURCE #-} qualified Game.GameItemList as GameItemList
import {-# SOURCE #-} qualified Game.GameUtil as GameUtil
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

initItems :: Quake ()
initItems = do
    gameBaseGlobals.gbGame.glNumItems .= V.length GameItemList.itemList -- RESEARCH: jake2 has -1 here...

{-
- ============ SpawnItem
-
- Sets the clipping size and plants the object on the floor.
-
- Items can't be immediately dropped to floor, because they might be on an
- entity that hasn't spawned yet. ============
-}
spawnItem :: EdictReference -> GItemReference -> Quake ()
spawnItem edictRef gir@(GItemReference itemIdx) = do
    precacheItem (Just gir)

    Just item <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let dprintf = gameImport^.giDprintf
        modelIndex = gameImport^.giModelIndex

    edict <- readEdictT edictRef

    when ((edict^.eSpawnFlags) /= 0 && (edict^.eClassName) == "key_power_cube") $ do
      modifyEdictT edictRef (\v -> v & eSpawnFlags .~ 0)
      dprintf $ (edict^.eClassName) `B.append` " at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` " has invalid spawnflags set\n"
      
    -- some items will be prevented in deathmatch
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    done <- if deathmatchValue /= 0
              then do
                dmFlagsValue :: Int <- liftM (truncate . (^.cvValue)) dmFlagsCVar

                let a = if dmFlagsValue .&. Constants.dfNoArmor /= 0
                          then case (item^.giPickup) of
                                 Just (PickupArmor _ _) -> True
                                 Just (PickupPowerArmor _ _) -> True
                                 _ -> False
                          else False

                    b = if dmFlagsValue .&. Constants.dfNoItems /= 0
                          then case (item^.giPickup) of
                                 Just (PickupArmor _ _) -> True
                                 _ -> False
                          else False

                    c = if dmFlagsValue .&. Constants.dfNoHealth /= 0
                          then case (item^.giPickup) of
                                 Just (PickupHealth _ _) -> True
                                 Just (PickupAdrenaline _ _) -> True
                                 Just (PickupAncientHead _ _) -> True
                                 _ -> False
                          else False

                    d = if dmFlagsValue .&. Constants.dfInfiniteAmmo /= 0
                          then if (item^.giFlags) == Constants.itAmmo || (edict^.eClassName) == "weapon_bfg"
                                 then True
                                 else False
                          else False

                return $ or [a, b, c, d]
              else return False

    if done
      then
        GameUtil.freeEdict edictRef 

      else do
        coopValue <- liftM (^.cvValue) coopCVar

        when (coopValue /= 0 && (edict^.eClassName) == "key_power_cube") $ do
          powerCubes <- use $ gameBaseGlobals.gbLevel.llPowerCubes
          modifyEdictT edictRef (\v -> v & eSpawnFlags %~ (.|. (1 `shiftL` (8 + powerCubes))))
          gameBaseGlobals.gbLevel.llPowerCubes += 1

        -- don't let them drop items that stay in a coop game
        when (coopValue /= 0 && ((item^.giFlags) .&. Constants.itStayCoop) /= 0) $
          gameBaseGlobals.gbItemList.ix itemIdx.giDrop .= Nothing

        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyEdictT edictRef (\v -> v & eItem .~ Just gir
                                       & eNextThink .~ levelTime + 2 * Constants.frameTime
                                       -- items start after other solids
                                       & eThink .~ Just dropToFloor
                                       & eEntityState.esEffects .~ (item^.giWorldModelFlags)
                                       & eEntityState.esRenderFx .~ Constants.rfGlow)

        when (isJust (edict^.eiModel)) $
          void (modelIndex (edict^.eiModel))

{-
- =============== PrecacheItem
-
- Precaches all data needed for a given item. This will be called for each
- item spawned in a level, and for each item in each client's inventory.
- ===============
-}
precacheItem :: Maybe GItemReference -> Quake ()
precacheItem it = do
    when (isJust it) $ do
      gameImport <- use $ gameBaseGlobals.gbGameImport

      let Just (GItemReference itemIdx) = it
          soundIndex = gameImport^.giSoundIndex
          modelIndex = gameImport^.giModelIndex
          imageIndex = gameImport^.giImageIndex

      Just item <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx

      when (isJust (item^.giPickupSound)) $
        void (soundIndex (item^.giPickupSound))

      when (isJust (item^.giWorldModel)) $
        void (modelIndex (item^.giWorldModel))

      when (isJust (item^.giViewModel)) $
        void (modelIndex (item^.giViewModel))

      when (isJust (item^.giIcon)) $
        void (imageIndex (item^.giIcon))

      -- parse everything for its ammo
      when (isJust (item^.giAmmo) && B.length (fromJust $ item^.giAmmo) > 0) $ do
        ammo <- findItem (fromJust $ item^.giAmmo)
        when (ammo /= it) $
          precacheItem ammo

      when (B.length (item^.giPrecaches) > 0) $ do
        let tokens = BC.split ' ' (item^.giPrecaches)
        mapM_ (precacheToken (item^.giPrecaches)) tokens

  where precacheToken :: B.ByteString -> B.ByteString -> Quake ()
        precacheToken fullPrecachesString token = do
          gameImport <- use $ gameBaseGlobals.gbGameImport

          let len = B.length token

          if len >= Constants.maxQPath || len < 5
            then do
              let err = gameImport^.giError
              err $ "PrecacheItem: it.classname has bad precache string: " `B.append` fullPrecachesString
            else -- determine type based on extension
              if | "md2" `BC.isSuffixOf` token -> do
                     let modelIndex = gameImport^.giModelIndex
                     void $ modelIndex (Just token)
                 | "sp2" `BC.isSuffixOf` token -> do
                     let modelIndex = gameImport^.giModelIndex
                     void $ modelIndex (Just token)
                 | "wav" `BC.isSuffixOf` token -> do
                     let soundIndex = gameImport^.giSoundIndex
                     void $ soundIndex (Just token)
                 | "pcx" `BC.isSuffixOf` token -> do
                     let imageIndex = gameImport^.giImageIndex
                     void $ imageIndex (Just token)
                 | otherwise -> do
                     let err = gameImport^.giError
                     err $ "PrecacheItem: bad precache string: " `B.append` token

{-
- =============== SetItemNames
-
- Called by worldspawn ===============
-}
setItemNames :: Quake ()
setItemNames = do
    numItems <- use $ gameBaseGlobals.gbGame.glNumItems
    mapM_ setConfigString [1..numItems-1]

    findItem "Jacket Armor" >>= (gameItemsGlobals.giJacketArmorIndex .=) . fromJust
    findItem "Combat Armor" >>= (gameItemsGlobals.giCombatArmorIndex .=) . fromJust
    findItem "Body Armor"   >>= (gameItemsGlobals.giBodyArmorIndex .=)   . fromJust
    findItem "Power Screen" >>= (gameItemsGlobals.giPowerScreenIndex .=) . fromJust
    findItem "Power Shield" >>= (gameItemsGlobals.giPowerShieldIndex .=) . fromJust

  where setConfigString :: Int -> Quake ()
        setConfigString idx = do
          Just item <- preuse $ gameBaseGlobals.gbItemList.ix idx
          configString <- use $ gameBaseGlobals.gbGameImport.giConfigString
          configString (Constants.csItems + idx) (fromJust (item^.giPickupName))

findItemByClassname :: B.ByteString -> Quake (Maybe GItemReference)
findItemByClassname className = do
    numItems <- use $ gameBaseGlobals.gbGame.glNumItems
    items <- use $ gameBaseGlobals.gbItemList
    return $ searchByClassName items (BC.map toLower className) 1 numItems

  where searchByClassName :: V.Vector GItemT -> B.ByteString -> Int -> Int -> Maybe GItemReference
        searchByClassName items classNameLower idx maxIdx
          | idx >= maxIdx = Nothing
          | otherwise =
              let item = items V.! idx
              in if classNameLower == BC.map toLower (item^.giClassName)
                   then Just (GItemReference idx)
                   else searchByClassName items classNameLower (idx + 1) maxIdx

findItem :: B.ByteString -> Quake (Maybe GItemReference)
findItem pickupName = do
    numItems <- use $ gameBaseGlobals.gbGame.glNumItems
    items <- use $ gameBaseGlobals.gbItemList
    let searchResult = searchByName items (BC.map toLower pickupName) 1 numItems

    when (isNothing searchResult) $
      Com.printf ("Item not found:" `B.append` pickupName `B.append` "\n")

    return searchResult

  where searchByName :: V.Vector GItemT -> B.ByteString -> Int -> Int -> Maybe GItemReference
        searchByName items name idx maxIdx
          | idx >= maxIdx = Nothing
          | otherwise =
              let item = items V.! idx
              in if name == BC.map toLower (fromJust $ item^.giPickupName) -- IMPROVE: this is actually bad, isn't it? what if pickup name is Nothing (in GItemT).. should be taken care of in all occasions
                then Just (GItemReference idx)
                else searchByName items name (idx + 1) maxIdx

jacketArmorInfo :: GItemArmorT
jacketArmorInfo =
  GItemArmorT { _giaBaseCount        = 25
              , _giaMaxCount         = 50
              , _giaNormalProtection = 0.3
              , _giaEnergyProtection = 0
              , _giaArmor            = Constants.armorJacket
              }

combatArmorInfo :: GItemArmorT
combatArmorInfo =
  GItemArmorT { _giaBaseCount        = 50
              , _giaMaxCount         = 100
              , _giaNormalProtection = 0.6
              , _giaEnergyProtection = 0.3
              , _giaArmor            = Constants.armorCombat
              }

bodyArmorInfo :: GItemArmorT
bodyArmorInfo =
  GItemArmorT { _giaBaseCount        = 100
              , _giaMaxCount         = 200
              , _giaNormalProtection = 0.8
              , _giaEnergyProtection = 0.6
              , _giaArmor            = Constants.armorBody
              }

pickupArmor :: EntInteract
pickupArmor = PickupArmor "pickup_armor" $ \edictRef otherRef -> do
  edict <- readEdictT edictRef
  let Just (GItemReference gItemIdx) = edict^.eItem

  -- get info on new armor
  Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
  let Just newInfo = gItem^.giInfo

  oldArmorIndex <- armorIndex otherRef

  other <- readEdictT otherRef
  let Just (GClientReference otherGClientIdx) = other^.eClient

  GItemReference jacketArmorIndex <- use $ gameItemsGlobals.giJacketArmorIndex

       -- handle armor shards specially
  if | (gItem^.giTag) == Constants.armorShard ->
         if oldArmorIndex == 0
           then gameBaseGlobals.gbGame.glClients.ix otherGClientIdx.gcPers.cpInventory.ix jacketArmorIndex .= 2
           else gameBaseGlobals.gbGame.glClients.ix otherGClientIdx.gcPers.cpInventory.ix oldArmorIndex += 2

       -- if player has no armor, just use it
     | oldArmorIndex == 0 ->
         gameBaseGlobals.gbGame.glClients.ix otherGClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) .= (newInfo^.giaBaseCount)

       -- use the better armor
     | otherwise -> do
         io (putStrLn "GameItems.pickupArmor") >> undefined -- TODO

  deathmatchValue <- liftM (^.cvValue) deathmatchCVar

  when ((edict^.eSpawnFlags) .&. Constants.droppedItem == 0 && deathmatchValue /= 0) $
    setRespawn edictRef 20

  return True

pickupPowerArmor :: EntInteract
pickupPowerArmor =
  PickupPowerArmor "pickup_powerarmor" $ \edictRef otherRef -> do
    other <- readEdictT otherRef
    let Just (GClientReference gClientIdx) = other^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    edict <- readEdictT edictRef
    let Just gItemRef@(GItemReference gItemIdx) = edict^.eItem
    Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx

    let quantity = (gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex)

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) += 1

    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    when (deathmatchValue /= 0) $ do
      when ((edict^.eSpawnFlags) .&. Constants.droppedItem == 0) $
        setRespawn edictRef (fromIntegral $ gItem^.giQuantity)

      when (quantity == 0) $
        itemUse (fromJust $ gItem^.giUse) otherRef gItemRef

    return True

usePowerArmor :: ItemUse
usePowerArmor =
  GenericItemUse "use_powerarmor" $ \edictRef _ -> do
    edict <- readEdictT edictRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex
        cprintf = gameImport^.giCprintf

    if (edict^.eFlags) .&. Constants.flPowerArmor /= 0
      then do
        modifyEdictT edictRef (\v -> v & eFlags %~ (.&. (complement Constants.flPowerArmor)))

        soundIdx <- soundIndex (Just "misc/power2.wav")
        sound (Just edictRef) Constants.chanAuto soundIdx 1 Constants.attnNorm 0

      else do
        Just (GItemReference cellsIdx) <- findItem "cells"
        Just cells <- preuse $ gameBaseGlobals.gbItemList.ix cellsIdx

        let Just (GClientReference gClientIdx) = edict^.eClient
        Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

        if (gClient^.gcPers.cpInventory) UV.! (cells^.giIndex) == 0
          then
            cprintf (Just edictRef) Constants.printHigh "No cells for power armor.\n"

          else do
            modifyEdictT edictRef (\v -> v & eFlags %~ (.|. Constants.flPowerArmor))

            soundIdx <- soundIndex (Just "misc/power1.wav")
            sound (Just edictRef) Constants.chanAuto soundIdx 1 Constants.attnNorm 0

dropPowerArmor :: ItemDrop
dropPowerArmor =
  GenericItemDrop "drop_powerarmor" $ \edictRef gItemRef@(GItemReference gItemIdx) -> do
    edict <- readEdictT edictRef
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx

    when ((edict^.eFlags) .&. Constants.flPowerArmor /= 0 && ((gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex)) == 1) $
      itemUse usePowerArmor edictRef gItemRef

    itemDrop dropGeneral edictRef gItemRef

pickupAmmo :: EntInteract
pickupAmmo =
  GenericEntInteract "pickup_ammo" $ \edictRef otherRef -> do
    edict <- readEdictT edictRef
    let Just gItemRef@(GItemReference gItemIdx) = edict^.eItem
    Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx

    other <- readEdictT otherRef
    let Just (GClientReference gClientIdx) = other^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    let weapon = (gItem^.giFlags) .&. Constants.itWeapon /= 0
        count = if | weapon && dmFlagsValue .&. Constants.dfInfiniteAmmo /= 0 -> 1000
                   | (edict^.eCount) /= 0 -> edict^.eCount
                   | otherwise -> gItem^.giQuantity
        oldCount = (gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex)

    added <- addAmmo otherRef gItemRef count

    if not added
      then
        return False

      else do
        deathmatchValue <- liftM (^.cvValue) deathmatchCVar

        when (weapon && oldCount == 0) $ do
          blasterIdx <- findItem "blaster"

          when ((gClient^.gcPers.cpWeapon) /= (edict^.eItem) && (deathmatchValue == 0 || (gClient^.gcPers.cpWeapon) == blasterIdx)) $
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcNewWeapon .= (edict^.eItem)
          
        when ((edict^.eSpawnFlags) .&. (Constants.droppedItem .|. Constants.droppedPlayerItem) == 0 && deathmatchValue /= 0) $
          setRespawn edictRef 30

        return True

dropAmmo :: ItemDrop
dropAmmo =
  GenericItemDrop "drop_ammo" $ \edictRef gItemRef@(GItemReference gItemIdx) -> do
    Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
    droppedRef <- dropItem edictRef gItemRef

    edict <- readEdictT edictRef
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

    let count = if (gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex) >= (gItem^.giQuantity)
                  then gItem^.giQuantity
                  else (gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex)

    modifyEdictT droppedRef (\v -> v & eCount .~ count)

    weapon <- case gClient^.gcPers.cpWeapon of
                Nothing -> return Nothing
                Just (GItemReference weaponIdx) -> preuse $ gameBaseGlobals.gbItemList.ix weaponIdx

    dropped <- readEdictT droppedRef

    if isJust weapon && ((fromJust weapon)^.giTag) == Constants.ammoGrenades && (gItem^.giTag) == Constants.ammoGrenades && ((gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex)) - (dropped^.eCount) <= 0
      then do
        cprintf <- use $ gameBaseGlobals.gbGameImport.giCprintf
        cprintf (Just edictRef) Constants.printHigh "Can't drop current weapon\n"
        GameUtil.freeEdict droppedRef

      else do
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) -= (dropped^.eCount)
        GameUtil.validateSelectedItem edictRef -- RESEARCH: why does jake2 has same validate method in Cmd (which is used here)

useQuad :: ItemUse
useQuad =
  GenericItemUse "use_quad" $ \edictRef (GItemReference gItemIdx) -> do
    edict <- readEdictT edictRef
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) -= 1
    GameUtil.validateSelectedItem edictRef

    quadDropTimeoutHack <- use $ gameItemsGlobals.giQuakeDropTimeoutHack

    timeout <- if quadDropTimeoutHack /= 0
                 then do
                   gameItemsGlobals.giQuakeDropTimeoutHack .= 0
                   return quadDropTimeoutHack
                 else
                   return 300

    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    levelFrameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

    let quadFrameNum = if (gClient^.gcQuadFrameNum) > fromIntegral levelFrameNum
                         then (gClient^.gcQuadFrameNum) + fromIntegral timeout
                         else fromIntegral (levelFrameNum + timeout)

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcQuadFrameNum .= quadFrameNum

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound

    soundIdx <- soundIndex (Just "items/damage.wav")
    sound (Just edictRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0

pickupPowerup :: EntInteract
pickupPowerup =
  GenericEntInteract "pickup_powerup" $ \edictRef otherRef -> do
    edict <- readEdictT edictRef
    other <- readEdictT otherRef
    let Just (GItemReference gItemIdx) = edict^.eItem
    let Just (GClientReference gClientIdx) = other^.eClient
    Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    skillValue <- liftM (^.cvValue) skillCVar
    coopValue <- liftM (^.cvValue) coopCVar

    let quantity = (gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex)

    if | skillValue == 1 && quantity >= 2 || skillValue >= 2 && quantity >= 1 ->
           return False

       | coopValue /= 0 && (gItem^.giFlags) .&. Constants.itStayCoop /= 0 && quantity > 0 ->
           return False

       | otherwise -> do
           deathmatchValue <- liftM (^.cvValue) deathmatchCVar

           when (deathmatchValue /= 0) $ do
             when ((edict^.eSpawnFlags) .&. Constants.droppedItem == 0) $
               setRespawn edictRef (fromIntegral $ gItem^.giQuantity)

             io (putStrLn "GameItems.pickupPowerup") >> undefined -- TODO

           return True

dropGeneral :: ItemDrop
dropGeneral =
  GenericItemDrop "drop_general" $ \edictRef gItemRef@(GItemReference gItemIdx) -> do
    dropItem edictRef gItemRef

    edict <- readEdictT edictRef
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) -= 1
    GameUtil.validateSelectedItem edictRef

useInvulnerability :: ItemUse
useInvulnerability =
  GenericItemUse "use_invulnerability" $ \edictRef (GItemReference gItemIdx) -> do
    edict <- readEdictT edictRef
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) -= 1
    GameUtil.validateSelectedItem edictRef

    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    levelFrameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

    let invincibleFrameNum = if (gClient^.gcInvincibleFrameNum) > fromIntegral levelFrameNum
                               then (gClient^.gcInvincibleFrameNum) + 300
                               else fromIntegral levelFrameNum + 300

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcInvincibleFrameNum .= invincibleFrameNum

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound

    soundIdx <- soundIndex (Just "items/protect.wav")
    sound (Just edictRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0

useSilencer :: ItemUse
useSilencer =
  GenericItemUse "use_silencer" $ \edictRef (GItemReference gItemIdx) -> do
    edict <- readEdictT edictRef
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) -= 1
    GameUtil.validateSelectedItem edictRef

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcSilencerShots += 30

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound

    soundIdx <- soundIndex (Just "items/damage.wav")
    sound (Just edictRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0

useBreather :: ItemUse
useBreather =
  GenericItemUse "use_breather" $ \edictRef (GItemReference gItemIdx) -> do
    edict <- readEdictT edictRef
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) -= 1
    GameUtil.validateSelectedItem edictRef

    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    levelFrameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

    let breatherFrameNum = if (gClient^.gcBreatherFrameNum) > fromIntegral levelFrameNum
                             then (gClient^.gcBreatherFrameNum) + 300
                             else fromIntegral levelFrameNum + 300

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcBreatherFrameNum .= breatherFrameNum

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound

    soundIdx <- soundIndex (Just "items/damage.wav")
    sound (Just edictRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0

useEnviroSuit :: ItemUse
useEnviroSuit =
  GenericItemUse "use_envirosuit" $ \edictRef (GItemReference gItemIdx) -> do
    edict <- readEdictT edictRef
    let Just (GClientReference gClientIdx) = edict^.eClient
    Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) -= 1
    GameUtil.validateSelectedItem edictRef

    Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
    levelFrameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum

    let enviroFrameNum = if (gClient^.gcEnviroFrameNum) > fromIntegral levelFrameNum
                           then (gClient^.gcEnviroFrameNum) + 300
                           else fromIntegral levelFrameNum + 300

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcEnviroFrameNum .= enviroFrameNum

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound

    soundIdx <- soundIndex (Just "items/damage.wav")
    sound (Just edictRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0

pickupAncientHead :: EntInteract
pickupAncientHead =
  PickupAncientHead "pickup_ancienthead" $ \edictRef otherRef -> do
    modifyEdictT otherRef (\v -> v & eMaxHealth +~ 2)

    edict <- readEdictT edictRef
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    when ((edict^.eSpawnFlags) .&. Constants.droppedItem == 0 && deathmatchValue /= 0) $ do
      let Just (GItemReference gItemIdx) = edict^.eItem
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      setRespawn edictRef (fromIntegral $ gItem^.giQuantity)

    return True

pickupAdrenaline :: EntInteract
pickupAdrenaline =
  PickupAdrenaline "pickup_adrenaline" $ \edictRef otherRef -> do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    when (deathmatchValue == 0) $
      modifyEdictT otherRef (\v -> v & eMaxHealth +~ 1)

    other <- readEdictT otherRef

    when ((other^.eHealth) < (other^.eMaxHealth)) $
      modifyEdictT otherRef (\v -> v & eHealth .~ (other^.eMaxHealth))

    edict <- readEdictT edictRef

    when ((edict^.eSpawnFlags) .&. Constants.droppedItem == 0 && deathmatchValue /= 0) $ do
      let Just (GItemReference gItemIdx) = edict^.eItem
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      setRespawn edictRef (fromIntegral $ gItem^.giQuantity)

    return True

pickupBandolier :: EntInteract
pickupBandolier =
  GenericEntInteract "pickup_bandolier" $ \edictRef otherRef -> do
    other <- readEdictT otherRef
    let Just gClientRef = other^.eClient

    checkMaxAmmo gClientRef

    checkBullets gClientRef
    checkShells gClientRef

    edict <- readEdictT edictRef
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    when ((edict^.eSpawnFlags) .&. Constants.droppedItem == 0 && deathmatchValue /= 0) $ do
      let Just (GItemReference gItemIdx) = edict^.eItem
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      setRespawn edictRef (fromIntegral $ gItem^.giQuantity)

    return True

  where checkMaxAmmo :: GClientReference -> Quake ()
        checkMaxAmmo (GClientReference gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          when ((gClient^.gcPers.cpMaxBullets) < 250) $
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpMaxBullets .= 250

          when ((gClient^.gcPers.cpMaxShells) < 150) $
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpMaxShells .= 150

          when ((gClient^.gcPers.cpMaxCells) < 250) $
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpMaxCells .= 250

          when ((gClient^.gcPers.cpMaxSlugs) < 75) $
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpMaxSlugs .= 75


pickupPack :: EntInteract
pickupPack =
  GenericEntInteract "pickup_pack" $ \edictRef otherRef -> do
    other <- readEdictT otherRef
    let Just gClientRef = other^.eClient

    checkMaxAmmo gClientRef

    checkBullets gClientRef
    checkShells gClientRef
    checkCells gClientRef
    checkGrenades gClientRef
    checkRockets gClientRef
    checkSlugs gClientRef

    edict <- readEdictT edictRef
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    when ((edict^.eSpawnFlags) .&. Constants.droppedItem == 0 && deathmatchValue /= 0) $ do
      let Just (GItemReference gItemIdx) = edict^.eItem
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      setRespawn edictRef (fromIntegral $ gItem^.giQuantity)

    return True
      
  where checkMaxAmmo :: GClientReference -> Quake ()
        checkMaxAmmo (GClientReference gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          when ((gClient^.gcPers.cpMaxBullets) < 300) $
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpMaxBullets .= 300

          when ((gClient^.gcPers.cpMaxShells) < 200) $
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpMaxShells .= 200

          when ((gClient^.gcPers.cpMaxRockets) < 100) $
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpMaxRockets .= 100

          when ((gClient^.gcPers.cpMaxGrenades) < 100) $
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpMaxGrenades .= 100

          when ((gClient^.gcPers.cpMaxCells) < 300) $
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpMaxCells .= 300

          when ((gClient^.gcPers.cpMaxSlugs) < 100) $
            gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpMaxSlugs .= 100

checkBullets :: GClientReference -> Quake ()
checkBullets (GClientReference gClientIdx) = do
  foundItem <- findItem "Bullets"

  case foundItem of
    Nothing ->
      return ()

    Just (GItemReference gItemIdx) -> do
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) %= (\v -> if v + (gItem^.giQuantity) > (gClient^.gcPers.cpMaxBullets) then gClient^.gcPers.cpMaxBullets else v + (gItem^.giQuantity))

checkShells :: GClientReference -> Quake ()
checkShells (GClientReference gClientIdx) = do
  foundItem <- findItem "Shells"

  case foundItem of
    Nothing ->
      return ()

    Just (GItemReference gItemIdx) -> do
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) %= (\v -> if v + (gItem^.giQuantity) > (gClient^.gcPers.cpMaxShells) then gClient^.gcPers.cpMaxShells else v + (gItem^.giQuantity))

checkCells :: GClientReference -> Quake ()
checkCells (GClientReference gClientIdx) = do
  foundItem <- findItem "Cells"

  case foundItem of
    Nothing ->
      return ()

    Just (GItemReference gItemIdx) -> do
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) %= (\v -> if v + (gItem^.giQuantity) > (gClient^.gcPers.cpMaxCells) then gClient^.gcPers.cpMaxCells else v + (gItem^.giQuantity))

checkGrenades :: GClientReference -> Quake ()
checkGrenades (GClientReference gClientIdx) = do
  foundItem <- findItem "Grenades"

  case foundItem of
    Nothing ->
      return ()

    Just (GItemReference gItemIdx) -> do
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) %= (\v -> if v + (gItem^.giQuantity) > (gClient^.gcPers.cpMaxGrenades) then gClient^.gcPers.cpMaxGrenades else v + (gItem^.giQuantity))

checkRockets :: GClientReference -> Quake ()
checkRockets (GClientReference gClientIdx) = do
  foundItem <- findItem "Rockets"

  case foundItem of
    Nothing ->
      return ()

    Just (GItemReference gItemIdx) -> do
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) %= (\v -> if v + (gItem^.giQuantity) > (gClient^.gcPers.cpMaxRockets) then gClient^.gcPers.cpMaxRockets else v + (gItem^.giQuantity))

checkSlugs :: GClientReference -> Quake ()
checkSlugs (GClientReference gClientIdx) = do
  foundItem <- findItem "Slugs"

  case foundItem of
    Nothing ->
      return ()

    Just (GItemReference gItemIdx) -> do
      Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
      Just gItem <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
      gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (gItem^.giIndex) %= (\v -> if v + (gItem^.giQuantity) > (gClient^.gcPers.cpMaxSlugs) then gClient^.gcPers.cpMaxSlugs else v + (gItem^.giQuantity))

pickupKey :: EntInteract
pickupKey =
  GenericEntInteract "pickup_key" $ \edictRef otherRef -> do
    coopValue <- liftM (^.cvValue) coopCVar
    edict <- readEdictT edictRef
    other <- readEdictT otherRef

    let Just (GClientReference gClientIdx) = other^.eClient
        Just (GItemReference gItemIdx) = edict^.eItem

    Just item <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx

    if coopValue /= 0
      then do
        Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

        if (edict^.eClassName) == "key_power_cube"
          then do
            if (gClient^.gcPers.cpPowerCubes) .&. (((edict^.eSpawnFlags) .&. 0xFF00) `shiftR` 8) /= 0
              then
                return False

              else do
                zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers) $ do
                  cpInventory.ix (item^.giIndex) += 1
                  cpPowerCubes %= (.|. (((edict^.eSpawnFlags) .&. 0xFF00) `shiftR` 8))

                return True

          else do
            if (gClient^.gcPers.cpInventory) UV.! (item^.giIndex) /= 0
              then
                return False

              else do
                gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (item^.giIndex) .= 1
                return True

      else do
        gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (item^.giIndex) += 1
        return True

pickupHealth :: EntInteract
pickupHealth = PickupHealth "pickup_health" $ \edictRef otherRef -> do
  edict <- readEdictT edictRef
  other <- readEdictT otherRef

  if (edict^.eStyle) .&. Constants.healthIgnoreMax == 0 && (other^.eHealth) >= (other^.eMaxHealth)
    then
      return False

    else do
      modifyEdictT otherRef (\v -> v & eHealth +~ (edict^.eCount))
      when ((edict^.eStyle) .&. Constants.healthIgnoreMax == 0) $
        modifyEdictT otherRef (\v -> v & eHealth %~ (\vv -> if vv > (other^.eMaxHealth) then other^.eMaxHealth else vv))

      if (edict^.eStyle) .&. Constants.healthTimed /= 0
        then do
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime

          modifyEdictT edictRef (\v -> v & eThink .~ Just GameUtil.megaHealthThink
                                         & eNextThink .~ levelTime + 5
                                         & eOwner .~ Just otherRef
                                         & eFlags %~ (.|. Constants.flRespawn)
                                         & eSvFlags %~ (.|. Constants.svfNoClient)
                                         & eSolid .~ Constants.solidNot)

        else do
          deathmatchValue <- liftM (^.cvValue) deathmatchCVar

          when ((edict^.eSpawnFlags) .&. Constants.droppedItem /= 0 && deathmatchValue /= 0) $
            setRespawn edictRef 30

      return True

-- QUAKED item_health (.3 .3 1) (-16 -16 -16) (16 16 16)
spItemHealth :: EdictReference -> Quake ()
spItemHealth edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    dmflags :: Int <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    if deathmatchValue /= 0 && (dmflags .&. Constants.dfNoHealth) /= 0
      then 
        GameUtil.freeEdict edictRef

      else do
        modifyEdictT edictRef (\v -> v & eiModel .~ Just "models/items/healing/medium/tris.md2"
                                       & eCount .~ 10)

        findItem "Health" >>= (spawnItem edictRef) . fromJust

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
        void $ soundIndex (Just "items/n_health.wav")

-- QUAKED item_health_small (.3 .3 1) (-16 -16 -16) (16 16 16)
spItemHealthSmall :: EdictReference -> Quake ()
spItemHealthSmall edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    dmflags :: Int <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    if deathmatchValue /= 0 && (dmflags .&. Constants.dfNoHealth) /= 0
      then 
        GameUtil.freeEdict edictRef 

      else do
        modifyEdictT edictRef (\v -> v & eiModel .~ Just "models/items/healing/stimpack/tris.md2"
                                       & eCount .~ 2)

        findItem "Health" >>= (spawnItem edictRef) . fromJust

        modifyEdictT edictRef (\v -> v & eStyle .~ Constants.healthIgnoreMax)

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
        void $ soundIndex (Just "items/s_health.wav")

-- QUAKED item_health_large (.3 .3 1) (-16 -16 -16) (16 16 16)
spItemHealthLarge :: EdictReference -> Quake ()
spItemHealthLarge edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    dmflags :: Int <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    if deathmatchValue /= 0 && (dmflags .&. Constants.dfNoHealth) /= 0
      then
        GameUtil.freeEdict edictRef

      else do
        modifyEdictT edictRef (\v -> v & eiModel .~ Just "models/items/healing/large/tris.md2"
                                       & eCount .~ 25)

        findItem "Health" >>= (spawnItem edictRef) . fromJust

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
        void $ soundIndex (Just "items/l_health.wav")

{-
- QUAKED item_health_mega (.3 .3 1) (-16 -16 -16) (16 16 16)
-}
spItemHealthMega :: EdictReference -> Quake ()
spItemHealthMega selfRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    dmFlagsValue <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    if deathmatchValue /= 0 && dmFlagsValue .&. Constants.dfNoHealth /= 0
      then
        GameUtil.freeEdict selfRef

      else do
        modifyEdictT selfRef (\v -> v & eiModel .~ Just "models/items/mega_h/tris.md2"
                                      & eCount .~ 100)

        Just health <- findItem "Health"
        spawnItem selfRef health

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
        soundIndex (Just "items/m_health.wav")

        modifyEdictT selfRef (\v -> v & eStyle .~ Constants.healthIgnoreMax .|. Constants.healthTimed)

dropToFloor :: EntThink
dropToFloor =
  GenericEntThink "drop_to_floor" $ \edictRef -> do
    modifyEdictT edictRef (\v -> v & eMins .~ V3 (-15) (-15) (-15)
                                   & eMaxs .~ V3 15 15 15)

    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        dprintf = gameImport^.giDprintf
        linkEntity = gameImport^.giLinkEntity
        trace = gameImport^.giTrace

    edict <- readEdictT edictRef

    if isJust (edict^.eiModel)
      then
        setModel edictRef (edict^.eiModel)

      else do
        let GItemReference itemIdx = fromJust $ edict^.eItem
        Just worldModel <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx.giWorldModel
        setModel edictRef worldModel

    modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidTrigger
                                   & eMoveType .~ Constants.moveTypeToss
                                   & eTouch .~ Just touchItem)
    
    let dest = (V3 0 0 (-128)) + (edict^.eEntityState.esOrigin)

    tr <- trace (edict^.eEntityState.esOrigin) (Just $ edict^.eMins) (Just $ edict^.eMaxs) dest (Just edictRef) Constants.maskSolid

    if tr^.tStartSolid
      then do
        dprintf $ "droptofloor: " `B.append` (edict^.eClassName) `B.append` " startsolid at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` "\n"
        GameUtil.freeEdict edictRef
      else do
        modifyEdictT edictRef (\v -> v & eEntityState.esOrigin .~ (tr^.tEndPos))

        when (isJust $ edict^.eTeam) $ do
          modifyEdictT edictRef (\v -> v & eFlags %~ (.&. (complement Constants.flTeamSlave))
                                         & eChain .~ (edict^.eTeamChain)
                                         & eTeamChain .~ Nothing
                                         & eSvFlags %~ (.|. Constants.svfNoClient)
                                         & eSolid .~ Constants.solidNot)

          when ((Just edictRef) == (edict^.eTeamMaster)) $ do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime

            modifyEdictT edictRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime
                                           & eThink .~ Just doRespawn)
            
        when ((edict^.eSpawnFlags) .&. Constants.itemNoTouch /= 0) $ do
          modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidBbox
                                         & eTouch .~ Nothing
                                         & eEntityState.esEffects %~ (.&. (complement Constants.efRotate))
                                         & eEntityState.esRenderFx %~ (.&. (complement Constants.rfGlow)))

        when ((edict^.eSpawnFlags) .&. Constants.itemTriggerSpawn /= 0) $ do
          modifyEdictT edictRef (\v -> v & eSvFlags %~ (.|. Constants.svfNoClient)
                                         & eSolid .~ Constants.solidNot
                                         & eUse .~ Just useItem)

        linkEntity edictRef

    return True

touchItem :: EntTouch
touchItem =
  GenericEntTouch "touch_item" $ \edictRef otherRef _ _ -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let imageIndex = gameImport^.giImageIndex
        sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex

    done <- shouldReturn edictRef otherRef

    unless done $ do
      taken <- runItemInteract edictRef otherRef
      edict <- readEdictT edictRef
      other <- readEdictT otherRef

      when taken $ do
        -- flash the screen
        let Just (GClientReference otherClientIdx) = other^.eClient
        gameBaseGlobals.gbGame.glClients.ix otherClientIdx.gcBonusAlpha .= 0.25

        -- show icon and name on status bar
        let Just (GItemReference itemIdx) = edict^.eItem
        Just item <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx
        icon <- imageIndex (item^.giIcon)
        time <- use $ gameBaseGlobals.gbLevel.llTime

        zoom (gameBaseGlobals.gbGame.glClients.ix otherClientIdx) $ do
          gcPlayerState.psStats.ix (Constants.statPickupIcon) .= fromIntegral icon
          gcPlayerState.psStats.ix (Constants.statPickupString) .= fromIntegral (Constants.csItems + (item^.giIndex))
          gcPickupMsgTime .= time + 3

        -- change selected item
        when (isJust $ item^.giUse) $ do
          zoom (gameBaseGlobals.gbGame.glClients.ix otherClientIdx) $ do
            gcPers.cpSelectedItem .= (item^.giIndex)
            gcPlayerState.psStats.ix (Constants.statSelectedItem) .= fromIntegral (item^.giIndex)

        case item^.giPickup of
          Just (PickupHealth _ _) -> do
            let count = edict^.eCount

            soundIdx <- if | count == 2 -> soundIndex (Just "items/s_health.wav")
                           | count == 10 -> soundIndex (Just "items/n_health.wav")
                           | count == 25 -> soundIndex (Just "items/l_health.wav")
                           | otherwise -> soundIndex (Just "items/m_health.wav")

            sound (Just otherRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0

          _ -> do
            when (isJust $ item^.giPickupSound) $ do
              pickupSound <- soundIndex (item^.giPickupSound)
              sound (Just otherRef) Constants.chanItem pickupSound 1 Constants.attnNorm 0

      let spawnFlags = edict^.eSpawnFlags

      when ((spawnFlags .&. Constants.itemTargetsUsed) == 0) $ do
        GameUtil.useTargets edictRef (Just otherRef)
        modifyEdictT edictRef (\v -> v & eSpawnFlags %~ (.|. Constants.itemTargetsUsed))

      when (taken) $ do
        coopValue <- liftM (^.cvValue) coopCVar
        let Just (GItemReference itemIdx) = edict^.eItem
        Just itemFlags <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx.giFlags
        edict' <- readEdictT edictRef
        let updatedSpawnFlags = edict'^.eSpawnFlags

        when (not (coopValue /= 0 && (itemFlags .&. Constants.itStayCoop) /= 0) || (updatedSpawnFlags .&. (Constants.droppedItem .|. Constants.droppedPlayerItem)) /= 0) $ do
          let flags = edict'^.eFlags
          if flags .&. Constants.flRespawn /= 0
            then modifyEdictT edictRef (\v -> v & eFlags %~ (.&. (complement Constants.flRespawn)))
            else GameUtil.freeEdict edictRef

  where shouldReturn :: EdictReference -> EdictReference -> Quake Bool
        shouldReturn edictRef otherRef = do
          edict <- readEdictT edictRef
          other <- readEdictT otherRef

          let Just (GItemReference itemIdx) = edict^.eItem
          Just pickup <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx.giPickup
                                           -- dead people can't pickup          -- not a grabbable item?
          if isNothing (other^.eClient) || (other^.eHealth) < 1 || isNothing pickup
            then return True
            else return False

        runItemInteract :: EdictReference -> EdictReference -> Quake Bool
        runItemInteract edictRef otherRef = do
          edict <- readEdictT edictRef
          let Just (GItemReference itemIdx) = edict^.eItem
          Just pickup <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx.giPickup

          entInteract (fromJust pickup) edictRef otherRef

doRespawn :: EntThink
doRespawn =
  GenericEntThink "do_respawn" $ \edictRef -> do
    edict <- readEdictT edictRef

    entRef <-
      if isJust (edict^.eTeam)
        then do
          let master = edict^.eTeamMaster

          count <- countDepth master 0
          choice <- Lib.rand

          let choice' = (fromIntegral choice) `mod` count :: Int

          makeChoice (fromJust master) 0 choice'
        else
          return edictRef

    modifyEdictT entRef (\v -> v & eSvFlags %~ (.&. (complement Constants.svfNoClient))
                                 & eSolid .~ Constants.solidTrigger)

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity entRef

    -- send an effect
    modifyEdictT entRef (\v -> v & eEntityState.esEvent .~ Constants.evItemRespawn)

    return False

  where countDepth :: Maybe EdictReference -> Int -> Quake Int
        countDepth Nothing count = return count
        countDepth (Just entRef) count = do
          ent <- readEdictT entRef
          countDepth (ent^.eChain) (count + 1)

        makeChoice :: EdictReference -> Int -> Int -> Quake EdictReference
        makeChoice entRef idx maxIdx
          | idx >= maxIdx = return entRef
          | otherwise = do
              ent <- readEdictT entRef
              makeChoice (fromJust $ ent^.eChain) (idx + 1) maxIdx

useItem :: EntUse
useItem =
  GenericEntUse "use_item" $ \edictRef _ _ -> do
    edict <- readEdictT edictRef

    let (solid, touch) = if (edict^.eSpawnFlags) .&. Constants.itemNoTouch /= 0
                           then (Constants.solidBbox, Nothing)
                           else (Constants.solidTrigger, Just touchItem)

    modifyEdictT edictRef (\v -> v & eSvFlags %~ (.&. (complement Constants.svfNoClient))
                                   & eUse .~ Nothing
                                   & eSolid .~ solid
                                   & eTouch .~ touch)

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity edictRef

dropItem :: EdictReference -> GItemReference -> Quake EdictReference
dropItem edictRef gItemRef@(GItemReference gItemIdx) = do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    droppedRef <- GameUtil.spawn
    edict <- readEdictT edictRef
    Just item <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        trace = gameImport^.giTrace
        linkEntity = gameImport^.giLinkEntity

    modifyEdictT droppedRef (\v -> v & eClassName .~ (item^.giClassName)
                                     & eItem .~ Just gItemRef
                                     & eSpawnFlags .~ Constants.droppedItem
                                     & eEntityState.esEffects .~ (item^.giWorldModelFlags)
                                     & eEntityState.esRenderFx .~ Constants.rfGlow
                                     & eMins .~ V3 (-15) (-15) (-15)
                                     & eMaxs .~ V3 15 15 15
                                     & eSolid .~ Constants.solidTrigger
                                     & eMoveType .~ Constants.moveTypeToss
                                     & eTouch .~ Just dropTempTouch
                                     & eOwner .~ Just edictRef)

    setModel droppedRef (item^.giWorldModel)
    
    forward <- case edict^.eClient of
                 Just (GClientReference gClientIdx) -> do
                   Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

                   let (Just forward, Just right, _) = Math3D.angleVectors (gClient^.gcVAngle) True True False
                       offset = V3 24 0 (-16)
                       origin = Math3D.projectSource (edict^.eEntityState.esOrigin) offset forward right
                   
                   modifyEdictT droppedRef (\v -> v & eEntityState.esOrigin .~ origin)
                   dropped <- readEdictT droppedRef

                   traceT <- trace (edict^.eEntityState.esOrigin) (Just $ dropped^.eMins) (Just $ dropped^.eMaxs) (dropped^.eEntityState.esOrigin) (Just edictRef) Constants.contentsSolid
                   modifyEdictT droppedRef (\v -> v & eEntityState.esOrigin .~ (traceT^.tEndPos))
                   return forward

                 Nothing -> do
                   let (Just forward, _, _) = Math3D.angleVectors (edict^.eEntityState.esAngles) True False False
                   modifyEdictT droppedRef (\v -> v & eEntityState.esOrigin .~ (edict^.eEntityState.esOrigin))
                   return forward

    let V3 a b c = fmap (* 100) forward
        velocity = V3 a b 300

    modifyEdictT droppedRef (\v -> v & eVelocity .~ velocity
                                     & eThink .~ Just dropMakeTouchable
                                     & eNextThink .~ levelTime + 1)

    linkEntity droppedRef

    return droppedRef

dropTempTouch :: EntTouch
dropTempTouch =
  GenericEntTouch "drop_temp_touch" $ \edictRef otherRef plane surf -> do
    edict <- readEdictT edictRef

    unless (Just otherRef == (edict^.eOwner)) $
      touch touchItem edictRef otherRef plane surf

dropMakeTouchable :: EntThink
dropMakeTouchable =
  GenericEntThink "drop_make_touchable" $ \edictRef -> do
    modifyEdictT edictRef (\v -> v & eTouch .~ Just touchItem)

    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    when (deathmatchValue /= 0) $ do
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      modifyEdictT edictRef (\v -> v & eNextThink .~ levelTime + 29
                                     & eThink .~ Just GameUtil.freeEdictA)

    return False

powerArmorType :: EdictReference -> Quake Int
powerArmorType edictRef = do
    edict <- readEdictT edictRef

    case edict^.eClient of
      Nothing ->
        return Constants.powerArmorNone

      Just (GClientReference gClientIdx) -> do
        Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

        GItemReference powerShieldIndex <- use $ gameItemsGlobals.giPowerShieldIndex
        GItemReference powerScreenIndex <- use $ gameItemsGlobals.giPowerScreenIndex

        itemList <- use $ gameBaseGlobals.gbItemList
        let powerShield = itemList V.! powerShieldIndex
            powerScreen = itemList V.! powerScreenIndex

        return $ if | (edict^.eFlags) .&. Constants.flPowerArmor == 0 -> Constants.powerArmorNone
                    | (gClient^.gcPers.cpInventory) UV.! (powerShield^.giIndex) > 0 -> Constants.powerArmorShield
                    | (gClient^.gcPers.cpInventory) UV.! (powerScreen^.giIndex) > 0 -> Constants.powerArmorScreen
                    | otherwise -> Constants.powerArmorNone

armorIndex :: EdictReference -> Quake Int
armorIndex edictRef = do
    edict <- readEdictT edictRef

    case edict^.eClient of
      Nothing ->
        return 0

      Just (GClientReference gClientIdx) -> do
        Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

        (GItemReference jacketArmorIndex) <- use $ gameItemsGlobals.giJacketArmorIndex
        (GItemReference combatArmorIndex) <- use $ gameItemsGlobals.giCombatArmorIndex
        (GItemReference bodyArmorIndex) <- use $ gameItemsGlobals.giBodyArmorIndex

        itemList <- use $ gameBaseGlobals.gbItemList
        let jacketArmor = itemList V.! jacketArmorIndex
            combatArmor = itemList V.! combatArmorIndex
            bodyArmor = itemList V.! bodyArmorIndex

        return $ if | (gClient^.gcPers.cpInventory) UV.! (jacketArmor^.giIndex) > 0 -> jacketArmor^.giIndex
                    | (gClient^.gcPers.cpInventory) UV.! (combatArmor^.giIndex) > 0 -> combatArmor^.giIndex
                    | (gClient^.gcPers.cpInventory) UV.! (bodyArmor^.giIndex) > 0 -> bodyArmor^.giIndex
                    | otherwise -> 0

getItemByIndex :: Int -> Quake (Maybe GItemT)
getItemByIndex index = do
    numItems <- use $ gameBaseGlobals.gbGame.glNumItems

    if index == 0 || index >= numItems
      then return Nothing
      else preuse $ gameBaseGlobals.gbItemList.ix index

setRespawn :: EdictReference -> Float -> Quake ()
setRespawn edictRef delay = do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT edictRef (\v -> v & eFlags %~ (.|. Constants.flRespawn)
                                   & eSvFlags %~ (.|. Constants.svfNoClient)
                                   & eSolid .~ Constants.solidNot
                                   & eNextThink .~ levelTime + delay
                                   & eThink .~ Just doRespawn)

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity edictRef

addAmmo :: EdictReference -> GItemReference -> Int -> Quake Bool
addAmmo edictRef (GItemReference gItemIdx) count = do
    edict <- readEdictT edictRef

    case edict^.eClient of
      Nothing ->
        return False

      Just (GClientReference gClientIdx) -> do
        Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
        Just item <- preuse $ gameBaseGlobals.gbItemList.ix gItemIdx

        let m = if | (item^.giTag) == Constants.ammoBullets -> Just (gClient^.gcPers.cpMaxBullets)
                   | (item^.giTag) == Constants.ammoShells -> Just (gClient^.gcPers.cpMaxShells)
                   | (item^.giTag) == Constants.ammoRockets -> Just (gClient^.gcPers.cpMaxRockets)
                   | (item^.giTag) == Constants.ammoGrenades -> Just (gClient^.gcPers.cpMaxGrenades)
                   | (item^.giTag) == Constants.ammoCells -> Just (gClient^.gcPers.cpMaxCells)
                   | (item^.giTag) == Constants.ammoSlugs -> Just (gClient^.gcPers.cpMaxSlugs)
                   | otherwise -> Nothing

        case m of
          Nothing ->
            return False

          Just max' -> do
            if (gClient^.gcPers.cpInventory) UV.! (item^.giIndex) == max'
              then
                return False

              else do
                gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (item^.giIndex) %= (\v -> if v + count > max' then max' else v + count)
                return True

selectNextItem :: EdictReference -> Int -> Quake ()
selectNextItem _ _ = do
    io (putStrLn "GameItems.selectNextItem") >> undefined -- TODO