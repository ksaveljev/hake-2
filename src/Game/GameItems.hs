{-# LANGUAGE OverloadedStrings #-}
module Game.GameItems where

import Control.Lens ((.=))
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Quake
import QuakeState
import Game.EntInteract
import Game.GItemArmorT
import Game.ItemDrop
import Game.ItemUse
import qualified Constants
import {-# SOURCE #-} qualified Game.GameItemList as GameItemList

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
- =============== SetItemNames
- 
- Called by worldspawn ===============
-}
setItemNames :: Quake ()
setItemNames = io (putStrLn "GameItems.setItemNames") >> undefined -- TODO

findItem :: B.ByteString -> Quake (Maybe Int) -- index of item from GameItemList.itemList
findItem _ = io (putStrLn "GameItems.findItem") >> undefined -- TODO

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
