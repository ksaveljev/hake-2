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
          eEdictAction.eaNextThink .= time + 2 * Constants.frameTime
          -- items start after other solids
          eEdictAction.eaThink .= Just dropToFloor
          eEntityState.esEffects .= (item^.giWorldModelFlags)
          eEntityState.esRenderFx .= Constants.rfGlow

        when (isJust (edict^.eEdictInfo.eiModel)) $
          void (modelIndex (fromJust $ edict^.eEdictInfo.eiModel))

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
        void (soundIndex $ fromJust (item^.giPickupSound))

      when (isJust (item^.giWorldModel)) $
        void (modelIndex $ fromJust (item^.giWorldModel))

      when (isJust (item^.giViewModel)) $
        void (modelIndex $ fromJust (item^.giViewModel))

      when (isJust (item^.giIcon)) $
        void (imageIndex $ fromJust (item^.giIcon))

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
                     void $ modelIndex token
                 | "sp2" `BC.isSuffixOf` token -> do
                     let modelIndex = gameImport^.giModelIndex
                     void $ modelIndex token
                 | "wav" `BC.isSuffixOf` token -> do
                     let soundIndex = gameImport^.giSoundIndex
                     void $ soundIndex token
                 | "pcx" `BC.isSuffixOf` token -> do
                     let imageIndex = gameImport^.giImageIndex
                     void $ imageIndex token
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

findItem :: B.ByteString -> Quake (Maybe GItemReference)
findItem pickupName = do
    numItems <- use $ gameBaseGlobals.gbGame.glNumItems
    searchResult <- searchByName (BC.map toLower pickupName) 1 numItems

    when (isNothing searchResult) $
      Com.printf ("Item not found:" `B.append` pickupName `B.append` "\n")

    return searchResult

  where searchByName :: B.ByteString -> Int -> Int -> Quake (Maybe GItemReference)
        searchByName name idx maxIdx
          | idx == maxIdx = return Nothing
          | otherwise = do
              Just item <- preuse $ gameBaseGlobals.gbItemList.ix idx
              if name == BC.map toLower (fromJust $ item^.giPickupName) -- IMPROVE: this is actually bad, isn't it? what if pickup name is Nothing (in GItemT).. should be taken care of in all occasions
                then return (Just $ GItemReference idx)
                else searchByName name (idx + 1) maxIdx

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
pickupArmor = PickupArmor "pickup_armor" undefined -- TODO

pickupPowerArmor :: EntInteract
pickupPowerArmor = PickupPowerArmor "pickup_powerarmor" undefined -- TODO

usePowerArmor :: ItemUse
usePowerArmor =
  GenericItemUse { _giuId = "use_powerarmor"
                 , _giuUse = undefined -- TODO
                 }

dropPowerArmor :: ItemDrop
dropPowerArmor =
  GenericItemDrop { _gidId = "drop_powerarmor"
                  , _gidDrop = undefined -- TODO
                  }

pickupAmmo :: EntInteract
pickupAmmo =
  GenericEntInteract { _geiId = "pickup_ammo"
                     , _geiInteract = undefined -- TODO
                     }

dropAmmo :: ItemDrop
dropAmmo =
  GenericItemDrop { _gidId = "drop_ammo"
                  , _gidDrop = undefined -- TODO
                  }

useQuad :: ItemUse
useQuad =
  GenericItemUse { _giuId = "use_quad"
                 , _giuUse = undefined -- TODO
                 }

pickupPowerup :: EntInteract
pickupPowerup =
  GenericEntInteract { _geiId = "pickup_powerup"
                     , _geiInteract = undefined -- TODO
                     }

dropGeneral :: ItemDrop
dropGeneral =
  GenericItemDrop { _gidId = "drop_general"
                  , _gidDrop = undefined -- TODO
                  }

useInvulnerability :: ItemUse
useInvulnerability =
  GenericItemUse { _giuId = "use_invulnerability"
                 , _giuUse = undefined -- TODO
                 }

useSilencer :: ItemUse
useSilencer =
  GenericItemUse { _giuId = "use_silencer"
                 , _giuUse = undefined -- TODO
                 }

useBreather :: ItemUse
useBreather =
  GenericItemUse { _giuId = "use_breather"
                 , _giuUse = undefined -- TODO
                 }

useEnviroSuit :: ItemUse
useEnviroSuit =
  GenericItemUse { _giuId = "use_envirosuit"
                 , _giuUse = undefined -- TODO
                 }

pickupAncientHead :: EntInteract
pickupAncientHead = PickupAncientHead "pickup_ancienthead" undefined -- TODO

pickupAdrenaline :: EntInteract
pickupAdrenaline = PickupAdrenaline "pickup_adrenaline" undefined -- TODO

pickupBandolier :: EntInteract
pickupBandolier =
  GenericEntInteract { _geiId = "pickup_bandolier"
                     , _geiInteract = undefined -- TODO
                     }

pickupPack :: EntInteract
pickupPack =
  GenericEntInteract { _geiId = "pickup_pack"
                     , _geiInteract = undefined -- TODO
                     }

pickupKey :: EntInteract
pickupKey =
  GenericEntInteract { _geiId = "pickup_key"
                     , _geiInteract = undefined -- TODO
                     }

pickupHealth :: EntInteract
pickupHealth = PickupHealth "pickup_health" undefined -- TODO

-- QUAKED item_health (.3 .3 1) (-16 -16 -16) (16 16 16)
spItemHealth :: EdictReference -> Quake ()
spItemHealth er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    dmflags :: Int <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    if deathmatchValue /= 0 && (dmflags .&. Constants.dfNoHealth) /= 0
      then GameUtil.freeEdict er
      else do
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEdictInfo.eiModel .= Just "models/items/healing/medium/tris.md2"
          eCount .= 10

        findItem "Health" >>= (spawnItem er) . fromJust

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
        void $ soundIndex "items/n_health.wav"

-- QUAKED item_health_small (.3 .3 1) (-16 -16 -16) (16 16 16)
spItemHealthSmall :: EdictReference -> Quake ()
spItemHealthSmall er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    dmflags :: Int <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    if deathmatchValue /= 0 && (dmflags .&. Constants.dfNoHealth) /= 0
      then GameUtil.freeEdict er
      else do
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEdictInfo.eiModel .= Just "models/items/healing/stimpack/tris.md2"
          eCount .= 2

        findItem "Health" >>= (spawnItem er) . fromJust

        gameBaseGlobals.gbGEdicts.ix edictIdx.eStyle .= Constants.healthIgnoreMax

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
        void $ soundIndex "items/s_health.wav"

-- QUAKED item_health_large (.3 .3 1) (-16 -16 -16) (16 16 16)
spItemHealthLarge :: EdictReference -> Quake ()
spItemHealthLarge er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    dmflags :: Int <- liftM (truncate . (^.cvValue)) dmFlagsCVar

    if deathmatchValue /= 0 && (dmflags .&. Constants.dfNoHealth) /= 0
      then GameUtil.freeEdict er
      else do
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEdictInfo.eiModel .= Just "models/items/healing/large/tris.md2"
          eCount .= 25

        findItem "Health" >>= (spawnItem er) . fromJust

        soundIndex <- use $ gameBaseGlobals.gbGameImport.giSoundIndex
        void $ soundIndex "items/l_health.wav"

spItemHealthMega :: EdictReference -> Quake ()
spItemHealthMega _ = io (putStrLn "GameItems.spItemHealthMega") >> undefined -- TODO

dropToFloor :: EntThink
dropToFloor =
  GenericEntThink "drop_to_floor" $ \er@(EdictReference edictIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictMinMax) $ do
      eMins .= V3 (-15) (-15) (-15)
      eMaxs .= V3 15 15 15

    gameImport <- use $ gameBaseGlobals.gbGameImport
    let setModel = gameImport^.giSetModel
        dprintf = gameImport^.giDprintf
        linkEntity = gameImport^.giLinkEntity
        trace = gameImport^.giTrace

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    if isJust (edict^.eEdictInfo.eiModel)
      then setModel er (edict^.eEdictInfo.eiModel)
      else do
        let GItemReference itemIdx = fromJust $ edict^.eItem
        Just worldModel <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx.giWorldModel
        setModel er worldModel

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eSolid .= Constants.solidTrigger
      eMoveType .= Constants.moveTypeToss
      eEdictAction.eaTouch .= Just touchItem

    let dest = (V3 0 0 (-128)) + (edict^.eEntityState.esOrigin)

    tr <- trace (edict^.eEntityState.esOrigin) (Just $ edict^.eEdictMinMax.eMins) (Just $ edict^.eEdictMinMax.eMaxs) dest er Constants.maskSolid

    if tr^.tStartSolid
      then do
        dprintf $ "droptofloor: " `B.append` (edict^.eClassName) `B.append` " startsolid at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` "\n"
        GameUtil.freeEdict er
      else do
        gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esOrigin .= (tr^.tEndPos)

        when (isJust $ edict^.eEdictInfo.eiTeam) $ do
          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
            eFlags %= (.&. (complement Constants.flTeamSlave))
            eEdictOther.eoChain .= (edict^.eEdictOther.eoTeamChain)
            eEdictOther.eoTeamChain .= Nothing
            eSvFlags %= (.|. Constants.svfNoClient)
            eSolid .= Constants.solidNot

          when ((Just er) == (edict^.eEdictOther.eoTeamMaster)) $ do
            time <- use $ gameBaseGlobals.gbLevel.llTime

            zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction) $ do
              eaNextThink .= time + Constants.frameTime
              eaThink .= Just doRespawn
            
        when ((edict^.eSpawnFlags) .&. Constants.itemNoTouch /= 0) $ do
          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
            eSolid .= Constants.solidBbox
            eEdictAction.eaTouch .= Nothing
            eEntityState.esEffects %= (.&. (complement Constants.efRotate))
            eEntityState.esRenderFx %= (.&. (complement Constants.rfGlow))

        when ((edict^.eSpawnFlags) .&. Constants.itemTriggerSpawn /= 0) $ do
          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
            eSvFlags %= (.|. Constants.svfNoClient)
            eSolid .= Constants.solidNot
            eEdictAction.eaUse .= Just useItem

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
        icon <- imageIndex (fromJust $ item^.giIcon)
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

            soundIdx <- if | count == 2 -> soundIndex "items/s_health.wav"
                           | count == 10 -> soundIndex "items/n_health.wav"
                           | count == 25 -> soundIndex "items/l_health.wav"
                           | otherwise -> soundIndex "items/m_health.wav"

            sound otherRef Constants.chanItem soundIdx 1 Constants.attnNorm 0

          _ -> do
            when (isJust $ item^.giPickupSound) $ do
              pickupSound <- soundIndex (fromJust $ item^.giPickupSound)
              sound otherRef Constants.chanItem pickupSound 1 Constants.attnNorm 0

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
          if isNothing (other^.eClient) || (other^.eEdictStatus.eHealth) < 1 || isNothing pickup
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
  GenericEntThink "do_respawn" $ \_ -> do
    io (putStrLn "GameItems.doRespawn") >> undefined -- TODO

useItem :: EntUse
useItem =
  GenericEntUse "use_item" $ \_ _ _ -> do
    io (putStrLn "GameItems.useItem") >> undefined -- TODO
