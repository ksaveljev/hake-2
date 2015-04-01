{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameItems where

import Control.Lens ((.=), (^.), use)
import Control.Monad (when, void)
import Data.Char (toLower)
import Data.Maybe (fromJust, isNothing, isJust)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import Game.EntInteract
import Game.GItemArmorT
import Game.ItemDrop
import Game.ItemUse
import qualified Constants
import {-# SOURCE #-} qualified Game.GameItemList as GameItemList
import qualified QCommon.Com as Com

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
spawnItem :: EdictReference -> Int -> Quake () -- second argument is index of GameItemList.itemList
spawnItem er@(EdictReference edictIdx) itemIdx = do
    io (putStrLn "GameItems.spawnItem") >> undefined -- TODO

{-
- =============== PrecacheItem
-
- Precaches all data needed for a given item. This will be called for each
- item spawned in a level, and for each item in each client's inventory.
- ===============
-}
precacheItem :: Maybe Int -> Quake () -- int is index of GameItemList.itemList
precacheItem it = do
    when (isJust it) $ do
      gameImport <- use $ gameBaseGlobals.gbGameImport

      let Just itemIdx = it
          item = GameItemList.itemList V.! itemIdx
          soundIndex = gameImport^.giSoundIndex
          modelIndex = gameImport^.giModelIndex
          imageIndex = gameImport^.giImageIndex

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
          let item = GameItemList.itemList V.! idx
          configString <- use $ gameBaseGlobals.gbGameImport.giConfigString
          configString (Constants.csItems + idx) (fromJust (item^.giPickupName))

findItem :: B.ByteString -> Quake (Maybe Int) -- index of item from GameItemList.itemList
findItem pickupName = do
    numItems <- use $ gameBaseGlobals.gbGame.glNumItems
    let searchResult = searchByName (BC.map toLower pickupName) 1 numItems

    when (isNothing searchResult) $
      Com.printf ("Item not found:" `B.append` pickupName `B.append` "\n")

    return searchResult

  where searchByName :: B.ByteString -> Int -> Int -> Maybe Int
        searchByName name idx maxIdx
          | idx == maxIdx = Nothing
          | otherwise =
              let item = GameItemList.itemList V.! idx
              in if name == BC.map toLower (fromJust $ item^.giPickupName) -- IMPROVE: this is actually bad, isn't it? what if pickup name is Nothing (in GItemT).. should be taken care of in all occasions
                   then Just idx
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
pickupArmor =
  GenericEntInteract { _geiId = "pickup_armor"
                     , _geiInteract = undefined -- TODO
                     }

pickupPowerArmor :: EntInteract
pickupPowerArmor =
  GenericEntInteract { _geiId = "pickup_powerarmor"
                     , _geiInteract = undefined -- TODO
                     }

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
pickupAncientHead =
  GenericEntInteract { _geiId = "pickup_ancienthead"
                     , _geiInteract = undefined -- TODO
                     }

pickupAdrenaline :: EntInteract
pickupAdrenaline =
  GenericEntInteract { _geiId = "pickup_adrenaline"
                     , _geiInteract = undefined -- TODO
                     }

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
pickupHealth =
  GenericEntInteract { _geiId = "pickup_health"
                     , _geiInteract = undefined -- TODO
                     }

spItemHealth :: EdictReference -> Quake ()
spItemHealth _ = io (putStrLn "GameItems.spItemHealth") >> undefined -- TODO

spItemHealthSmall :: EdictReference -> Quake ()
spItemHealthSmall _ = io (putStrLn "GameItems.spItemHealthSmall") >> undefined -- TODO

spItemHealthLarge :: EdictReference -> Quake ()
spItemHealthLarge _ = io (putStrLn "GameItems.spItemHealthLarge") >> undefined -- TODO

spItemHealthMega :: EdictReference -> Quake ()
spItemHealthMega _ = io (putStrLn "GameItems.spItemHealthMega") >> undefined -- TODO
