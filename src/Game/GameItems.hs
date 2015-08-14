{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Game.GameItems where

import Control.Lens ((.=), (^.), use, preuse, ix, (%=), (+=), zoom)
import Control.Monad (when, void, liftM, unless)
import Data.Bits ((.&.), (.|.), shiftL, complement)
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
import Game.GItemArmorT
import qualified Constants
import {-# SOURCE #-} qualified Game.GameItemList as GameItemList
import qualified Game.GameUtil as GameUtil
import qualified QCommon.Com as Com
import qualified Util.Lib as Lib

initItems :: Quake ()
initItems = do
    gameBaseGlobals.gbGame.glNumItems .= V.length GameItemList.itemList -- TODO: jake2 has -1 here...

{-
- ============ SpawnItem
-
- Sets the clipping size and plants the object on the floor.
-
- Items can't be immediately dropped to floor, because they might be on an
- entity that hasn't spawned yet. ============
-}
spawnItem :: EdictReference -> GItemReference -> Quake ()
spawnItem er@(EdictReference edictIdx) gir@(GItemReference itemIdx) = do
    precacheItem (Just gir)

    Just item <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx

    gameImport <- use $ gameBaseGlobals.gbGameImport
    let dprintf = gameImport^.giDprintf
        modelIndex = gameImport^.giModelIndex

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    when ((edict^.eSpawnFlags) /= 0 && (edict^.eClassName) == "key_power_cube") $ do
      gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags .= 0
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
      then GameUtil.freeEdict er
      else do
        coopValue <- liftM (^.cvValue) coopCVar

        when (coopValue /= 0 && (edict^.eClassName) == "key_power_cube") $ do
          powerCubes <- use $ gameBaseGlobals.gbLevel.llPowerCubes
          gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags %= (.|. (1 `shiftL` (8 + powerCubes)))
          gameBaseGlobals.gbLevel.llPowerCubes += 1

        -- don't let them drop items that stay in a coop game
        when (coopValue /= 0 && ((item^.giFlags) .&. Constants.itStayCoop) /= 0) $
          gameBaseGlobals.gbItemList.ix itemIdx.giDrop .= Nothing

        time <- use $ gameBaseGlobals.gbLevel.llTime

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eItem .= Just gir
          eNextThink .= time + 2 * Constants.frameTime
          -- items start after other solids
          eThink .= Just dropToFloor
          eEntityState.esEffects .= (item^.giWorldModelFlags)
          eEntityState.esRenderFx .= Constants.rfGlow

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
              , _giaEnergyProteciton = 0
              , _giaArmor            = Constants.armorJacket
              }

combatArmorInfo :: GItemArmorT
combatArmorInfo =
  GItemArmorT { _giaBaseCount        = 50
              , _giaMaxCount         = 100
              , _giaNormalProtection = 0.6
              , _giaEnergyProteciton = 0.3
              , _giaArmor            = Constants.armorCombat
              }

bodyArmorInfo :: GItemArmorT
bodyArmorInfo =
  GItemArmorT { _giaBaseCount        = 100
              , _giaMaxCount         = 200
              , _giaNormalProtection = 0.8
              , _giaEnergyProteciton = 0.6
              , _giaArmor            = Constants.armorBody
              }

pickupArmor :: EntInteract
pickupArmor = PickupArmor "pickup_armor" $ \_ _ -> do
  io (putStrLn "GameItems.pickupArmor") >> undefined -- TODO

pickupPowerArmor :: EntInteract
pickupPowerArmor = PickupPowerArmor "pickup_powerarmor" $ \_ _ -> do
  io (putStrLn "GameItems.pickupPowerArmor") >> undefined -- TODO

usePowerArmor :: ItemUse
usePowerArmor =
  GenericItemUse "use_powerarmor" $ \_ _ -> do
    io (putStrLn "GameItems.usePowerArmor") >> undefined -- TODO

dropPowerArmor :: ItemDrop
dropPowerArmor =
  GenericItemDrop "drop_powerarmor" $ \_ _ -> do
    io (putStrLn "GameItems.dropPowerArmor") >> undefined -- TODO

pickupAmmo :: EntInteract
pickupAmmo =
  GenericEntInteract "pickup_ammo" $ \_ _ -> do
    io (putStrLn "GameItems.pickupAmmo") >> undefined -- TODO

dropAmmo :: ItemDrop
dropAmmo =
  GenericItemDrop "drop_ammo" $ \_ _ -> do
    io (putStrLn "GameItems.dropAmmo") >> undefined -- TODO

useQuad :: ItemUse
useQuad =
  GenericItemUse "use_quad" $ \_ _ -> do
    io (putStrLn "GameItems.useQuad") >> undefined -- TODO

pickupPowerup :: EntInteract
pickupPowerup =
  GenericEntInteract "pickup_powerup" $ \_ _ -> do
    io (putStrLn "GameItems.pickupPowerup") >> undefined -- TODO

dropGeneral :: ItemDrop
dropGeneral =
  GenericItemDrop "drop_general" $ \_ _ -> do
    io (putStrLn "GameItems.dropGeneral") >> undefined -- TODO

useInvulnerability :: ItemUse
useInvulnerability =
  GenericItemUse "use_invulnerability" $ \_ _ -> do
    io (putStrLn "GameItems.useInvulnerability") >> undefined -- TODO

useSilencer :: ItemUse
useSilencer =
  GenericItemUse "use_silencer" $ \_ _ -> do
    io (putStrLn "GameItems.useSilencer") >> undefined -- TODO

useBreather :: ItemUse
useBreather =
  GenericItemUse "use_breather" $ \_ _ -> do
    io (putStrLn "GameItems.useBreather") >> undefined -- TODO

useEnviroSuit :: ItemUse
useEnviroSuit =
  GenericItemUse "use_envirosuit" $ \_ _ -> do
    io (putStrLn "GameItems.useEnviroSuit") >> undefined -- TODO

pickupAncientHead :: EntInteract
pickupAncientHead = PickupAncientHead "pickup_ancienthead" $ \_ _ -> do
  io (putStrLn "GameItems.pickupAncientHead") >> undefined -- TODO

pickupAdrenaline :: EntInteract
pickupAdrenaline = PickupAdrenaline "pickup_adrenaline" $ \_ _ -> do
  io (putStrLn "GameItems.pickupAdrenaline") >> undefined -- TODO

pickupBandolier :: EntInteract
pickupBandolier =
  GenericEntInteract "pickup_bandolier" $ \_ _ -> do
    io (putStrLn "GameItems.pickupBandolier") >> undefined -- TODO

pickupPack :: EntInteract
pickupPack =
  GenericEntInteract "pickup_pack" $ \_ _ -> do
    io (putStrLn "GameItems.pickupPack") >> undefined -- TODO

pickupKey :: EntInteract
pickupKey =
  GenericEntInteract "pickup_key" $ \_ _ -> do
    io (putStrLn "GameItems.pickupKey") >> undefined -- TODO

pickupHealth :: EntInteract
pickupHealth = PickupHealth "pickup_health" $ \_ _ -> do
  io (putStrLn "GameItems.pickupHealth") >> undefined -- TODO

-- QUAKED item_health (.3 .3 1) (-16 -16 -16) (16 16 16)
spItemHealth :: EdictReference -> Quake ()
spItemHealth er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    dmflags :: Int <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    if deathmatchValue /= 0 && (dmflags .&. Constants.dfNoHealth) /= 0
      then GameUtil.freeEdict er
      else do
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eiModel .= Just "models/items/healing/medium/tris.md2"
          eCount .= 10

        findItem "Health" >>= (spawnItem er) . fromJust

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
        void $ soundIndex (Just "items/n_health.wav")

-- QUAKED item_health_small (.3 .3 1) (-16 -16 -16) (16 16 16)
spItemHealthSmall :: EdictReference -> Quake ()
spItemHealthSmall er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    dmflags :: Int <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    if deathmatchValue /= 0 && (dmflags .&. Constants.dfNoHealth) /= 0
      then GameUtil.freeEdict er
      else do
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eiModel .= Just "models/items/healing/stimpack/tris.md2"
          eCount .= 2

        findItem "Health" >>= (spawnItem er) . fromJust

        gameBaseGlobals.gbGEdicts.ix edictIdx.eStyle .= Constants.healthIgnoreMax

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
        void $ soundIndex (Just "items/s_health.wav")

-- QUAKED item_health_large (.3 .3 1) (-16 -16 -16) (16 16 16)
spItemHealthLarge :: EdictReference -> Quake ()
spItemHealthLarge er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    dmflags :: Int <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    if deathmatchValue /= 0 && (dmflags .&. Constants.dfNoHealth) /= 0
      then GameUtil.freeEdict er
      else do
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eiModel .= Just "models/items/healing/large/tris.md2"
          eCount .= 25

        findItem "Health" >>= (spawnItem er) . fromJust

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
        void $ soundIndex (Just "items/l_health.wav")

spItemHealthMega :: EdictReference -> Quake ()
spItemHealthMega _ = io (putStrLn "GameItems.spItemHealthMega") >> undefined -- TODO

dropToFloor :: EntThink
dropToFloor =
  GenericEntThink "drop_to_floor" $ \er@(EdictReference edictIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMins .= V3 (-15) (-15) (-15)
      eMaxs .= V3 15 15 15

    gameImport <- use $ gameBaseGlobals.gbGameImport
    let setModel = gameImport^.giSetModel
        dprintf = gameImport^.giDprintf
        linkEntity = gameImport^.giLinkEntity
        trace = gameImport^.giTrace

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    if isJust (edict^.eiModel)
      then setModel er (edict^.eiModel)
      else do
        let GItemReference itemIdx = fromJust $ edict^.eItem
        Just worldModel <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx.giWorldModel
        setModel er worldModel

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eSolid .= Constants.solidTrigger
      eMoveType .= Constants.moveTypeToss
      eTouch .= Just touchItem

    let dest = (V3 0 0 (-128)) + (edict^.eEntityState.esOrigin)

    tr <- trace (edict^.eEntityState.esOrigin) (Just $ edict^.eMins) (Just $ edict^.eMaxs) dest (Just er) Constants.maskSolid

    if tr^.tStartSolid
      then do
        dprintf $ "droptofloor: " `B.append` (edict^.eClassName) `B.append` " startsolid at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` "\n"
        GameUtil.freeEdict er
      else do
        gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin .= (tr^.tEndPos)

        when (isJust $ edict^.eTeam) $ do
          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
            eFlags %= (.&. (complement Constants.flTeamSlave))
            eChain .= (edict^.eTeamChain)
            eTeamChain .= Nothing
            eSvFlags %= (.|. Constants.svfNoClient)
            eSolid .= Constants.solidNot

          when ((Just er) == (edict^.eTeamMaster)) $ do
            time <- use $ gameBaseGlobals.gbLevel.llTime

            zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
              eNextThink .= time + Constants.frameTime
              eThink .= Just doRespawn
            
        when ((edict^.eSpawnFlags) .&. Constants.itemNoTouch /= 0) $ do
          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
            eSolid .= Constants.solidBbox
            eTouch .= Nothing
            eEntityState.esEffects %= (.&. (complement Constants.efRotate))
            eEntityState.esRenderFx %= (.&. (complement Constants.rfGlow))

        when ((edict^.eSpawnFlags) .&. Constants.itemTriggerSpawn /= 0) $ do
          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
            eSvFlags %= (.|. Constants.svfNoClient)
            eSolid .= Constants.solidNot
            eUse .= Just useItem

        linkEntity er

    return True

touchItem :: EntTouch
touchItem =
  GenericEntTouch "touch_item" $ \edictRef@(EdictReference edictIdx) otherRef@(EdictReference otherIdx) _ _ -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let imageIndex = gameImport^.giImageIndex
        sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex

    done <- shouldReturn edictRef otherRef

    unless done $ do
      taken <- runItemInteract edictRef otherRef

      when taken $ do
        -- flash the screen
        Just otherClientRef <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx.eClient
        let Just (GClientReference otherClientIdx) = otherClientRef
        gameBaseGlobals.gbGame.glClients.ix otherClientIdx.gcBonusAlpha .= 0.25

        -- show icon and name on status bar
        Just itemRef <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eItem
        let Just (GItemReference itemIdx) = itemRef
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
            Just count <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eCount

            soundIdx <- if | count == 2 -> soundIndex (Just "items/s_health.wav")
                           | count == 10 -> soundIndex (Just "items/n_health.wav")
                           | count == 25 -> soundIndex (Just "items/l_health.wav")
                           | otherwise -> soundIndex (Just "items/m_health.wav")

            sound (Just otherRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0

          _ -> do
            when (isJust $ item^.giPickupSound) $ do
              pickupSound <- soundIndex (item^.giPickupSound)
              sound (Just otherRef) Constants.chanItem pickupSound 1 Constants.attnNorm 0

      Just spawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags

      when ((spawnFlags .&. Constants.itemTargetsUsed) == 0) $ do
        GameUtil.useTargets edictRef (Just otherRef)
        gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags %= (.|. Constants.itemTargetsUsed)

      when (taken) $ do
        coopValue <- liftM (^.cvValue) coopCVar
        Just itemRef <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eItem
        let Just (GItemReference itemIdx) = itemRef
        Just itemFlags <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx.giFlags
        Just updatedSpawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags

        when (not (coopValue /= 0 && (itemFlags .&. Constants.itStayCoop) /= 0) || (updatedSpawnFlags .&. (Constants.droppedItem .|. Constants.droppedPlayerItem)) /= 0) $ do
          Just flags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eFlags
          if flags .&. Constants.flRespawn /= 0
            then gameBaseGlobals.gbGEdicts.ix edictIdx.eFlags %= (.&. (complement Constants.flRespawn))
            else GameUtil.freeEdict edictRef

  where shouldReturn :: EdictReference -> EdictReference -> Quake Bool
        shouldReturn (EdictReference edictIdx) (EdictReference otherIdx) = do
          Just other <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx
          Just itemRef <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eItem
          let Just (GItemReference itemIdx) = itemRef
          Just pickup <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx.giPickup
                                           -- dead people can't pickup          -- not a grabbable item?
          if isNothing (other^.eClient) || (other^.eHealth) < 1 || isNothing pickup
            then return True
            else return False

        runItemInteract :: EdictReference -> EdictReference -> Quake Bool
        runItemInteract edictRef@(EdictReference edictIdx) otherRef = do
          Just itemRef <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eItem
          let Just (GItemReference itemIdx) = itemRef
          Just pickup <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx.giPickup

          entInteract (fromJust pickup) edictRef otherRef

doRespawn :: EntThink
doRespawn =
  GenericEntThink "do_respawn" $ \edictRef@(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    entRef@(EdictReference entIdx) <-
      if isJust (edict^.eTeam)
        then do
          edicts <- use $ gameBaseGlobals.gbGEdicts
          let master = edict^.eTeamMaster
              count = countDepth edicts master 0

          choice <- Lib.rand
          let choice' = (fromIntegral choice) `mod` count :: Int

          return (makeChoice edicts (fromJust master) 0 choice')
        else
          return edictRef

    zoom (gameBaseGlobals.gbGEdicts.ix entIdx) $ do
      eSvFlags %= (.&. (complement Constants.svfNoClient))
      eSolid .= Constants.solidTrigger

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity entRef

    -- send an effect
    gameBaseGlobals.gbGEdicts.ix entIdx.eEntityState.esEvent .= Constants.evItemRespawn

    return False

  where countDepth :: V.Vector EdictT -> Maybe EdictReference -> Int -> Int
        countDepth _ Nothing count = count
        countDepth edicts (Just (EdictReference entIdx)) count =
          let ent = edicts V.! entIdx
          in countDepth edicts (ent^.eChain) (count + 1)

        makeChoice :: V.Vector EdictT -> EdictReference -> Int -> Int -> EdictReference
        makeChoice edicts entRef@(EdictReference entIdx) idx maxIdx
          | idx >= maxIdx = entRef
          | otherwise =
              let ent = edicts V.! entIdx
              in makeChoice edicts (fromJust $ ent^.eChain) (idx + 1) maxIdx

useItem :: EntUse
useItem =
  GenericEntUse "use_item" $ \_ _ _ -> do
    io (putStrLn "GameItems.useItem") >> undefined -- TODO

dropItem :: EdictReference -> GItemReference -> Quake EdictReference
dropItem _ _ = do
    io (putStrLn "GameItems.dropItem") >> undefined -- TODO

powerArmorType :: EdictReference -> Quake Int
powerArmorType (EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    case edict^.eClient of
      Nothing -> return Constants.powerArmorNone
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
armorIndex (EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    case edict^.eClient of
      Nothing -> return 0
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
