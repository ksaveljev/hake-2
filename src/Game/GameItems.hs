module Game.GameItems
  ( bodyArmorInfo
  , combatArmorInfo
  , dropAmmo
  , dropGeneral
  , dropPowerArmor
  , findItem
  , jacketArmorInfo
  , pickupAdrenaline
  , pickupAmmo
  , pickupAncientHead
  , pickupArmor
  , pickupBandolier
  , pickupHealth
  , pickupKey
  , pickupPack
  , pickupPowerArmor
  , pickupPowerup
  , useBreather
  , useEnviroSuit
  , useInvulnerability
  , usePowerArmor
  , useQuad
  , useSilencer
  ) where

import qualified Constants
import qualified QCommon.Com as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types

import           Control.Lens (use, ix, (^.), (&), (.~), (+~), (%~))
import           Control.Monad (when)
import           Data.Bits (complement, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as UV

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
pickupArmor = EntInteract "pickupArmor" pickupArmorF

pickupArmorF :: EdictRef -> EdictRef -> Quake Bool
pickupArmorF edictRef otherRef =
  do edict <- readRef edictRef
     other <- readRef otherRef
     proceedPickupArmor edictRef edict otherRef (other^.eClient) (edict^.eItem)

proceedPickupArmor :: EdictRef -> EdictT -> EdictRef -> Maybe GClientRef -> Maybe GItemRef -> Quake Bool
proceedPickupArmor _ _ _ Nothing _ = pickupError "GameItems.pickupArmor gClientRef is Nothing"
proceedPickupArmor _ _ _ _ Nothing = pickupError "GameItems.pickupArmor gItemRef is Nothing"
proceedPickupArmor edictRef edict otherRef (Just gClientRef) (Just gItemRef) =
  do gItem <- readRef gItemRef
     oldArmorIndex <- armorIndex otherRef
     jacketArmorIndex <- use (gameItemsGlobals.giJacketArmorIndex)
     done <- handleArmor gClientRef gItem jacketArmorIndex oldArmorIndex (gItem^.giInfo)
     deathmatchValue >>= applyDeathmatchActions done
  where deathmatchValue = fmap (^.cvValue) deathmatchCVar
        applyDeathmatchActions False _ = return False
        applyDeathmatchActions True dmv
          | (edict^.eSpawnFlags) .&. Constants.droppedItem == 0 && dmv /= 0 =
              do setRespawn edictRef 20
                 return True
          | otherwise = return True
          
pickupError :: B.ByteString -> Quake Bool
pickupError msg = Com.fatalError msg >> return True

handleArmor :: GClientRef -> GItemT -> GItemRef -> Int -> Maybe GItemArmorT -> Quake Bool
handleArmor _ _ _ _ Nothing = pickupError "GameItems.pickupArmor gItem^.giInfo is Nothing"
handleArmor gClientRef gItem (GItemRef jacketArmorIndex) oldArmorIndex (Just newInfo)
  | isArmorShard && oldArmorIndex == 0 =
      updateGClient (\v -> v & gcPers.cpInventory.ix jacketArmorIndex .~ 2)
  | isArmorShard =
      updateGClient (\v -> v & gcPers.cpInventory.ix oldArmorIndex +~ 2)
  | oldArmorIndex == 0 =
      updateGClient (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) .~ (newInfo^.giaBaseCount))
  | otherwise =
      useBetterArmor gClientRef gItem oldArmorIndex newInfo
  where isArmorShard = (gItem^.giTag) == Constants.armorShard
        updateGClient f = modifyRef gClientRef f >> return True

useBetterArmor :: GClientRef -> GItemT -> Int -> GItemArmorT -> Quake Bool
useBetterArmor gClientRef gItem oldArmorIndex newInfo =
  do gClient <- readRef gClientRef
     getOldInfo >>= applyArmorValues gClient
  where getOldInfo =
          do jacketArmorIndex <- use (gameItemsGlobals.giJacketArmorIndex)
             combatArmorIndex <- use (gameItemsGlobals.giCombatArmorIndex)
             return (checkOldArmorIndex jacketArmorIndex combatArmorIndex)
        checkOldArmorIndex (GItemRef jacketArmorIndex) (GItemRef combatArmorIndex)
          | oldArmorIndex == jacketArmorIndex = jacketArmorInfo
          | oldArmorIndex == combatArmorIndex = combatArmorInfo
          | otherwise = bodyArmorInfo
        applyArmorValues gClient oldInfo
          | (newInfo^.giaNormalProtection) > (oldInfo^.giaNormalProtection) =
              applyNewArmor gClientRef gClient gItem oldInfo newInfo oldArmorIndex
          | otherwise =
              updateOldArmor gClientRef gClient oldInfo newInfo oldArmorIndex

applyNewArmor :: GClientRef -> GClientT -> GItemT -> GItemArmorT -> GItemArmorT -> Int -> Quake Bool
applyNewArmor gClientRef gClient gItem oldInfo newInfo oldArmorIndex =
    do modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix oldArmorIndex .~ 0
                                & gcPers.cpInventory.ix (gItem^.giIndex) .~ newCount)
       return True
  where salvage = (oldInfo^.giaNormalProtection) / (newInfo^.giaNormalProtection)
        salvageCount = truncate salvage * ((gClient^.gcPers.cpInventory) UV.! oldArmorIndex)
        count = (newInfo^.giaBaseCount) + salvageCount
        newCount = min count (newInfo^.giaMaxCount)

updateOldArmor :: GClientRef -> GClientT -> GItemArmorT -> GItemArmorT -> Int -> Quake Bool
updateOldArmor gClientRef gClient oldInfo newInfo oldArmorIndex
  | currentCount >= newCount = return False
  | otherwise =
      do modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix oldArmorIndex .~ newCount)
         return True
  where currentCount = (gClient^.gcPers.cpInventory) UV.! oldArmorIndex
        salvage = (newInfo^.giaNormalProtection) / (oldInfo^.giaNormalProtection)
        salvageCount = truncate salvage * (newInfo^.giaBaseCount)
        count = currentCount + salvageCount
        newCount = min count (oldInfo^.giaMaxCount)

pickupPowerArmor :: EntInteract
pickupPowerArmor = EntInteract "pickupPowerArmor" pickupPowerArmorF

pickupPowerArmorF :: EdictRef -> EdictRef -> Quake Bool
pickupPowerArmorF edictRef otherRef =
  do edict <- readRef edictRef
     other <- readRef otherRef
     proceedPickupPowerArmor edictRef edict otherRef (other^.eClient) (edict^.eItem)
     return True
     
proceedPickupPowerArmor :: EdictRef -> EdictT -> EdictRef -> Maybe GClientRef -> Maybe GItemRef -> Quake ()
proceedPickupPowerArmor _ _ _ Nothing _ = Com.fatalError "GameItems.pickupPowerArmor gClientRef is Nothing"
proceedPickupPowerArmor _ _ _ _ Nothing = Com.fatalError "GameItems.pickupPowerArmor gItemRef is Nothing"
proceedPickupPowerArmor edictRef edict otherRef (Just gClientRef) (Just gItemRef) =
  do gItem <- readRef gItemRef
     modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) +~ 1)
     deathmatchValue >>= applyDeathmatchActions gItem
  where deathmatchValue = fmap (^.cvValue) deathmatchCVar
        applyDeathmatchActions gItem dmv
          | dmv == 0 = return ()
          | otherwise =
              do gClient <- readRef gClientRef
                 when ((edict^.eSpawnFlags) .&. Constants.droppedItem == 0) $
                   setRespawn edictRef (fromIntegral (gItem^.giQuantity))
                 when ((gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex) == 0) $
                   itemUseAction (gItem^.giUse) otherRef gItemRef

itemUseAction :: Maybe ItemUse -> EdictRef -> GItemRef -> Quake ()
itemUseAction Nothing _ _ = Com.fatalError "GameItems.itemUseAction with Nothing"
itemUseAction (Just useAction) edictRef gItemRef =
  itemUse useAction edictRef gItemRef

usePowerArmor :: ItemUse
usePowerArmor = ItemUse "usePowerArmor" usePowerArmorF

usePowerArmorF :: EdictRef -> GItemRef -> Quake ()
usePowerArmorF edictRef _ =
  do edict <- readRef edictRef
     gameImport <- use (gameBaseGlobals.gbGameImport)
     proceedUsePowerArmor edictRef edict gameImport

proceedUsePowerArmor :: EdictRef -> EdictT -> GameImportT -> Quake ()
proceedUsePowerArmor edictRef edict gameImport
  | (edict^.eFlags) .&. Constants.flPowerArmor /= 0 =
      do modifyRef edictRef (\v -> v & eFlags %~ (.&. complement Constants.flPowerArmor))
         soundIdx <- soundIndex (Just "misc/power2.wav")
         sound (Just edictRef) Constants.chanAuto soundIdx 1 Constants.attnNorm 0
  | otherwise =
      do cellsRef <- findItem "cells"
         checkCellsForPowerArmor edictRef gameImport cellsRef (edict^.eClient)
  where sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex
        
checkCellsForPowerArmor :: EdictRef -> GameImportT -> Maybe GItemRef -> Maybe GClientRef -> Quake ()
checkCellsForPowerArmor _ _ Nothing _ = Com.fatalError "GameItems.usePowerArmor cellsRef is Nothing"
checkCellsForPowerArmor _ _ _ Nothing = Com.fatalError "GameItems.usePowerArmor gClientRef is Nothing"
checkCellsForPowerArmor edictRef gameImport (Just cellsRef) (Just gClientRef) =
  do cells <- readRef cellsRef
     gClient <- readRef gClientRef
     applyPowerArmor edictRef gameImport gClient cells

applyPowerArmor :: EdictRef -> GameImportT -> GClientT -> GItemT -> Quake ()
applyPowerArmor edictRef gameImport gClient cells
  | (gClient^.gcPers.cpInventory) UV.! (cells^.giIndex) == 0 =
      cprintf (Just edictRef) Constants.printHigh "No cells for power armor.\n"
  | otherwise =
      do modifyRef edictRef (\v -> v & eFlags %~ (.|. Constants.flPowerArmor))
         soundIdx <- soundIndex (Just "misc/power1.wav")
         sound (Just edictRef) Constants.chanAuto soundIdx 1 Constants.attnNorm 0
  where cprintf = gameImport^.giCprintf
        sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex

dropPowerArmor :: ItemDrop
dropPowerArmor = ItemDrop "dropPowerArmor" dropPowerArmorF

dropPowerArmorF :: EdictRef -> GItemRef -> Quake ()
dropPowerArmorF edictRef gItemRef =
  do edict <- readRef edictRef
     proceedDropPowerArmor edictRef edict gItemRef (edict^.eClient)
     
proceedDropPowerArmor :: EdictRef -> EdictT -> GItemRef -> Maybe GClientRef -> Quake ()
proceedDropPowerArmor _ _ _ Nothing = Com.fatalError "GameItems.dropPowerArmor GClientRef is Nothing"
proceedDropPowerArmor edictRef edict gItemRef (Just gClientRef) =
  do gClient <- readRef gClientRef
     gItem <- readRef gItemRef
     when ((edict^.eFlags) .&. Constants.flPowerArmor /= 0 && ((gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex)) == 1) $
       itemUse usePowerArmor edictRef gItemRef
     itemDrop dropGeneral edictRef gItemRef

pickupAmmo :: EntInteract
pickupAmmo = error "GameItems.pickupAmmo" -- TODO

dropAmmo :: ItemDrop
dropAmmo = error "GameItems.dropAmmo" -- TODO

useQuad :: ItemUse
useQuad = error "GameItems.useQuad" -- TODO

pickupPowerup :: EntInteract
pickupPowerup = error "GameItems.pickupPowerup" -- TODO

dropGeneral :: ItemDrop
dropGeneral = error "GameItems.dropGeneral" -- TODO

useInvulnerability :: ItemUse
useInvulnerability = error "GameItems.useInvulnerability" -- TODO

useSilencer :: ItemUse
useSilencer = error "GameItems.useSilencer" -- TODO

useBreather :: ItemUse
useBreather = error "GameItems.useBreather" -- TODO

useEnviroSuit :: ItemUse
useEnviroSuit = error "GameItems.useEnviroSuit" -- TODO

pickupAncientHead :: EntInteract
pickupAncientHead = error "GameItems.pickupAncientHead" -- TODO

pickupAdrenaline :: EntInteract
pickupAdrenaline = error "GameItems.pickupAdrenaline" -- TODO

pickupBandolier :: EntInteract
pickupBandolier = error "GameItems.pickupBandolier" -- TODO

pickupPack :: EntInteract
pickupPack = error "GameItems.pickupPack" -- TODO

pickupKey :: EntInteract
pickupKey = error "GameItems.pickupKey" -- TODO

pickupHealth :: EntInteract
pickupHealth = error "GameItems.pickupHealth" -- TODO

findItem :: B.ByteString -> Quake (Maybe GItemRef)
findItem = error "GameItems.findItem" -- TODO

setRespawn :: EdictRef -> Float -> Quake ()
setRespawn = error "GameItems.setRespawn" -- TODO

armorIndex :: EdictRef -> Quake Int
armorIndex = error "GameItems.armorIndex" -- TODO