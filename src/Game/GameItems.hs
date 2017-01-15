{-# LANGUAGE FlexibleContexts #-}
module Game.GameItems
  ( bodyArmorInfo
  , combatArmorInfo
  , dropAmmo
  , dropGeneral
  , dropPowerArmor
  , findItem
  , initItems
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
  , spawnItem
  , spItemHealth
  , spItemHealthLarge
  , spItemHealthMega
  , spItemHealthSmall
  , useBreather
  , useEnviroSuit
  , useInvulnerability
  , usePowerArmor
  , useQuad
  , useSilencer
  ) where

import qualified Constants
import           Game.ClientPersistantT
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import {-# SOURCE #-} qualified Game.GameItemsList as GameItemsList
import           Game.GameLocalsT
import qualified Game.GameUtil as GameUtil
import           Game.GClientT
import           Game.GItemArmorT
import           Game.GItemT
import           Game.LevelLocalsT
import           Game.TraceT
import qualified QCommon.Com as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

import           Control.Lens (use, ix, (^.), (.=), (&), (.~), (+~), (-~), (%~))
import           Control.Monad (when, unless, void)
import           Data.Bits (complement, shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (toLower)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Linear (V3(..), _z)

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

pickupArmorF :: Ref' EdictT -> Ref' EdictT -> Quake Bool
pickupArmorF edictRef otherRef =
  do edict <- readRef edictRef
     other <- readRef otherRef
     proceedPickupArmor edictRef edict otherRef (other^.eClient) (edict^.eItem)

proceedPickupArmor :: Ref' EdictT -> EdictT -> Ref' EdictT -> Maybe (Ref' GClientT) -> Maybe (Ref' GItemT) -> Quake Bool
proceedPickupArmor _ _ _ Nothing _ = pickupError "GameItems.pickupArmor gClientRef is Nothing"
proceedPickupArmor _ _ _ _ Nothing = pickupError "GameItems.pickupArmor gItemRef is Nothing"
proceedPickupArmor edictRef edict otherRef (Just gClientRef) (Just gItemRef) =
  do gItem <- readRef gItemRef
     oldArmorIndex <- armorIndex otherRef
     jacketArmorIndex <- use (gameItemsGlobals.giJacketArmorIndex)
     done <- handleArmor gClientRef gItem jacketArmorIndex oldArmorIndex (gItem^.giInfo)
     applyDeathmatchActions done =<< deathmatchValue
  where deathmatchValue = fmap (^.cvValue) deathmatchCVar
        applyDeathmatchActions False _ = return False
        applyDeathmatchActions True dmv
          | (edict^.eSpawnFlags) .&. Constants.droppedItem == 0 && dmv /= 0 =
              do setRespawn edictRef 20
                 return True
          | otherwise = return True
          
pickupError :: B.ByteString -> Quake Bool
pickupError msg = Com.fatalError msg >> return True

handleArmor :: Ref' GClientT -> GItemT -> Ref' GItemT -> Int -> Maybe GItemArmorT -> Quake Bool
handleArmor _ _ _ _ Nothing = pickupError "GameItems.pickupArmor gItem^.giInfo is Nothing"
handleArmor gClientRef gItem (Ref jacketArmorIndex) oldArmorIndex (Just newInfo)
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

useBetterArmor :: Ref' GClientT -> GItemT -> Int -> GItemArmorT -> Quake Bool
useBetterArmor gClientRef gItem oldArmorIndex newInfo =
  do gClient <- readRef gClientRef
     getOldInfo >>= applyArmorValues gClient
  where getOldInfo =
          do jacketArmorIndex <- use (gameItemsGlobals.giJacketArmorIndex)
             combatArmorIndex <- use (gameItemsGlobals.giCombatArmorIndex)
             return (checkOldArmorIndex jacketArmorIndex combatArmorIndex)
        checkOldArmorIndex (Ref jacketArmorIndex) (Ref combatArmorIndex)
          | oldArmorIndex == jacketArmorIndex = jacketArmorInfo
          | oldArmorIndex == combatArmorIndex = combatArmorInfo
          | otherwise = bodyArmorInfo
        applyArmorValues gClient oldInfo
          | (newInfo^.giaNormalProtection) > (oldInfo^.giaNormalProtection) =
              applyNewArmor gClientRef gClient gItem oldInfo newInfo oldArmorIndex
          | otherwise =
              updateOldArmor gClientRef gClient oldInfo newInfo oldArmorIndex

applyNewArmor :: Ref' GClientT -> GClientT -> GItemT -> GItemArmorT -> GItemArmorT -> Int -> Quake Bool
applyNewArmor gClientRef gClient gItem oldInfo newInfo oldArmorIndex =
    do modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix oldArmorIndex .~ 0
                                & gcPers.cpInventory.ix (gItem^.giIndex) .~ newCount)
       return True
  where salvage = (oldInfo^.giaNormalProtection) / (newInfo^.giaNormalProtection)
        salvageCount = truncate salvage * ((gClient^.gcPers.cpInventory) UV.! oldArmorIndex)
        count = (newInfo^.giaBaseCount) + salvageCount
        newCount = min count (newInfo^.giaMaxCount)

updateOldArmor :: Ref' GClientT -> GClientT -> GItemArmorT -> GItemArmorT -> Int -> Quake Bool
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

pickupPowerArmorF :: Ref' EdictT -> Ref' EdictT -> Quake Bool
pickupPowerArmorF edictRef otherRef =
  do edict <- readRef edictRef
     other <- readRef otherRef
     proceedPickupPowerArmor edictRef edict otherRef (other^.eClient) (edict^.eItem)
     return True
     
proceedPickupPowerArmor :: Ref' EdictT -> EdictT -> Ref' EdictT -> Maybe (Ref' GClientT) -> Maybe (Ref' GItemT) -> Quake ()
proceedPickupPowerArmor _ _ _ Nothing _ = Com.fatalError "GameItems.pickupPowerArmor gClientRef is Nothing"
proceedPickupPowerArmor _ _ _ _ Nothing = Com.fatalError "GameItems.pickupPowerArmor gItemRef is Nothing"
proceedPickupPowerArmor edictRef edict otherRef (Just gClientRef) (Just gItemRef) =
  do gItem <- readRef gItemRef
     modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) +~ 1)
     applyDeathmatchActions gItem =<< deathmatchValue
  where deathmatchValue = fmap (^.cvValue) deathmatchCVar
        applyDeathmatchActions gItem dmv
          | dmv == 0 = return ()
          | otherwise =
              do gClient <- readRef gClientRef
                 when ((edict^.eSpawnFlags) .&. Constants.droppedItem == 0) $
                   setRespawn edictRef (fromIntegral (gItem^.giQuantity))
                 when ((gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex) == 0) $
                   itemUseAction (gItem^.giUse) otherRef gItemRef

itemUseAction :: Maybe ItemUse -> Ref' EdictT -> Ref' GItemT -> Quake ()
itemUseAction Nothing _ _ = Com.fatalError "GameItems.itemUseAction with Nothing"
itemUseAction (Just useAction) edictRef gItemRef =
  itemUse useAction edictRef gItemRef

usePowerArmor :: ItemUse
usePowerArmor = ItemUse "usePowerArmor" usePowerArmorF

usePowerArmorF :: Ref' EdictT -> Ref' GItemT -> Quake ()
usePowerArmorF edictRef _ =
  do edict <- readRef edictRef
     gameImport <- use (gameBaseGlobals.gbGameImport)
     proceedUsePowerArmor edictRef edict gameImport

proceedUsePowerArmor :: Ref' EdictT -> EdictT -> GameImportT -> Quake ()
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
        
checkCellsForPowerArmor :: Ref' EdictT -> GameImportT -> Maybe (Ref' GItemT) -> Maybe (Ref' GClientT) -> Quake ()
checkCellsForPowerArmor _ _ Nothing _ = Com.fatalError "GameItems.usePowerArmor cellsRef is Nothing"
checkCellsForPowerArmor _ _ _ Nothing = Com.fatalError "GameItems.usePowerArmor gClientRef is Nothing"
checkCellsForPowerArmor edictRef gameImport (Just cellsRef) (Just gClientRef) =
  do cells <- readRef cellsRef
     gClient <- readRef gClientRef
     applyPowerArmor edictRef gameImport gClient cells

applyPowerArmor :: Ref' EdictT -> GameImportT -> GClientT -> GItemT -> Quake ()
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

dropPowerArmorF :: Ref' EdictT -> Ref' GItemT -> Quake ()
dropPowerArmorF edictRef gItemRef =
  do edict <- readRef edictRef
     proceedDropPowerArmor edictRef edict gItemRef (edict^.eClient)
     
proceedDropPowerArmor :: Ref' EdictT -> EdictT -> Ref' GItemT -> Maybe (Ref' GClientT) -> Quake ()
proceedDropPowerArmor _ _ _ Nothing = Com.fatalError "GameItems.dropPowerArmor GClientRef is Nothing"
proceedDropPowerArmor edictRef edict gItemRef (Just gClientRef) =
  do gClient <- readRef gClientRef
     gItem <- readRef gItemRef
     when ((edict^.eFlags) .&. Constants.flPowerArmor /= 0 && ((gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex)) == 1) $
       itemUse usePowerArmor edictRef gItemRef
     itemDrop dropGeneral edictRef gItemRef

pickupAmmo :: EntInteract
pickupAmmo = EntInteract "pickupAmmo" pickupAmmoF

pickupAmmoF :: Ref' EdictT -> Ref' EdictT -> Quake Bool
pickupAmmoF edictRef otherRef =
  do edict <- readRef edictRef
     other <- readRef otherRef
     proceedPickupAmmo edictRef edict otherRef (other^.eClient) (edict^.eItem)

proceedPickupAmmo :: Ref' EdictT -> EdictT -> Ref' EdictT -> Maybe (Ref' GClientT) -> Maybe (Ref' GItemT) -> Quake Bool
proceedPickupAmmo _ _ _ Nothing _ = pickupError "GameItems.pickupAmmo GClientRef is Nothing"
proceedPickupAmmo _ _ _ _ Nothing = pickupError "GameItems.pickupAmmo GItemRef is Nothing"
proceedPickupAmmo edictRef edict otherRef (Just gClientRef) (Just gItemRef) =
  do gItem <- readRef gItemRef
     gClient <- readRef gClientRef
     calcAndAddAmmo edictRef edict otherRef gClientRef gClient gItemRef gItem =<< dmFlagsValue
  where dmFlagsValue = fmap (truncate . (^.cvValue)) dmFlagsCVar

calcAndAddAmmo :: Ref' EdictT -> EdictT -> Ref' EdictT -> Ref' GClientT -> GClientT -> Ref' GItemT -> GItemT -> Int -> Quake Bool
calcAndAddAmmo edictRef edict otherRef gClientRef gClient gItemRef gItem dmfv =
  do added <- addAmmo otherRef gItemRef count
     equipWeaponAndSetRespawn added
  where isWeapon = (gItem^.giFlags) .&. Constants.itWeapon /= 0
        count | isWeapon && dmfv .&. Constants.dfInfiniteAmmo /= 0 = 1000
              | (edict^.eCount) /= 0 = edict^.eCount
              | otherwise = gItem^.giQuantity
        oldCount = (gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex)
        equipWeaponAndSetRespawn False = return False
        equipWeaponAndSetRespawn True =
          do dmv <- deathmatchValue
             equipBlaster dmv
             respawnPickedUpItem dmv
             return True
        deathmatchValue = fmap (^.cvValue) deathmatchCVar
        equipBlaster dmv
          | isWeapon && oldCount == 0 =
              do blasterRef <- findItem "blaster"
                 when ((gClient^.gcPers.cpWeapon) /= (edict^.eItem) && (dmv == 0 || (gClient^.gcPers.cpWeapon) == blasterRef)) $
                   modifyRef gClientRef (\v -> v & gcNewWeapon .~ (edict^.eItem))
          | otherwise = return ()
        respawnPickedUpItem dmv
          | (edict^.eSpawnFlags) .&. (Constants.droppedItem .|. Constants.droppedPlayerItem) == 0 && dmv /= 0 =
              setRespawn edictRef 30
          | otherwise = return ()

dropAmmo :: ItemDrop
dropAmmo = ItemDrop "dropAmmo" dropAmmoF

dropAmmoF :: Ref' EdictT -> Ref' GItemT -> Quake ()
dropAmmoF edictRef gItemRef =
  do gItem <- readRef gItemRef
     droppedEdictRef <- dropItem edictRef gItemRef
     edict <- readRef edictRef
     proceedDropAmmo edictRef gItem droppedEdictRef (edict^.eClient)

proceedDropAmmo :: Ref' EdictT -> GItemT -> Ref' EdictT -> Maybe (Ref' GClientT) -> Quake ()
proceedDropAmmo _ _ _ Nothing = Com.fatalError "GameItems.dropAmmo GClientRef is Nothing"
proceedDropAmmo edictRef gItem droppedEdictRef (Just gClientRef) =
  readRef gClientRef >>= calcAndDropAmmo edictRef gItem droppedEdictRef gClientRef

calcAndDropAmmo :: Ref' EdictT -> GItemT -> Ref' EdictT -> Ref' GClientT -> GClientT -> Quake ()
calcAndDropAmmo edictRef gItem droppedEdictRef gClientRef gClient =
  do modifyRef droppedEdictRef (\v -> v & eCount .~ count)
     droppedEdict <- readRef droppedEdictRef
     maybe (finishDropAmmo droppedEdict) (checkIfCanDropWeapon droppedEdict) (gClient^.gcPers.cpWeapon)
  where count | currentAmmoCount >= (gItem^.giQuantity) = gItem^.giQuantity
              | otherwise = currentAmmoCount
        currentAmmoCount = (gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex)
        finishDropAmmo droppedEdict =
          do modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) -~ (droppedEdict^.eCount))
             GameUtil.validateSelectedItem edictRef -- RESEARCH: why does jake2 has same validate method in Cmd (which is used here)
        checkIfCanDropWeapon droppedEdict weaponRef =
          do weapon <- readRef weaponRef
             dropWeaponOrAmmo droppedEdict weapon
        dropWeaponOrAmmo droppedEdict weapon
          | (weapon^.giTag) == Constants.ammoGrenades && (gItem^.giTag) == Constants.ammoGrenades && ((gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex)) - (droppedEdict^.eCount) <= 0 =
              do cprintf <- use (gameBaseGlobals.gbGameImport.giCprintf)
                 cprintf (Just edictRef) Constants.printHigh "Can't drop current weapon\n"
                 GameUtil.freeEdict droppedEdictRef
          | otherwise = finishDropAmmo droppedEdict

useQuad :: ItemUse
useQuad = ItemUse "useQuad" useQuadF

useQuadF :: Ref' EdictT -> Ref' GItemT -> Quake ()
useQuadF edictRef gItemRef =
  do edict <- readRef edictRef
     gItem <- readRef gItemRef
     proceedUseQuad edictRef gItem (edict^.eClient)

proceedUseQuad :: Ref' EdictT -> GItemT -> Maybe (Ref' GClientT) -> Quake ()
proceedUseQuad _ _ Nothing = Com.fatalError "GameItems.useQuad GClientRef is Nothing"
proceedUseQuad edictRef gItem (Just gClientRef) =
  do modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) -~ 1)
     GameUtil.validateSelectedItem edictRef
     timeout <- getTimeoutValue =<< quadDropTimeoutHack
     gClient <- readRef gClientRef
     quadFrameNum <- getQuadFrameNumValue gClient timeout <$> levelFrameNum
     modifyRef gClientRef (\v -> v & gcQuadFrameNum .~ quadFrameNum)
     playItemSound edictRef "items/damage.wav" =<< gameImport
  where quadDropTimeoutHack = use (gameItemsGlobals.giQuakeDropTimeoutHack)
        levelFrameNum = use (gameBaseGlobals.gbLevel.llFrameNum)
        gameImport = use (gameBaseGlobals.gbGameImport)

getQuadFrameNumValue :: GClientT -> Int -> Int -> Float
getQuadFrameNumValue gClient timeout frameNum
  | (gClient^.gcQuadFrameNum) > fromIntegral frameNum =
      (gClient^.gcQuadFrameNum) + fromIntegral timeout
  | otherwise = fromIntegral (frameNum + timeout)

getTimeoutValue :: Int -> Quake Int
getTimeoutValue timeoutHack
  | timeoutHack /= 0 =
      do gameItemsGlobals.giQuakeDropTimeoutHack .= 0
         return timeoutHack
  | otherwise = return 300

pickupPowerup :: EntInteract
pickupPowerup = EntInteract "pickupPowerup" pickupPowerupF

pickupPowerupF :: Ref' EdictT -> Ref' EdictT -> Quake Bool
pickupPowerupF edictRef otherRef =
  do edict <- readRef edictRef
     other <- readRef otherRef
     proceedPickupPowerup edictRef edict otherRef (other^.eClient) (edict^.eItem)

proceedPickupPowerup :: Ref' EdictT -> EdictT -> Ref' EdictT -> Maybe (Ref' GClientT) -> Maybe (Ref' GItemT) -> Quake Bool
proceedPickupPowerup _ _ _ Nothing _ = pickupError "GameItems.pickupPowerup GClientRef is Nothing"
proceedPickupPowerup _ _ _ _ Nothing = pickupError "GameItems.pickupPowerup GItemRef is Nothing"
proceedPickupPowerup edictRef edict otherRef (Just gClientRef) (Just gItemRef) =
  do gItem <- readRef gItemRef
     gClient <- readRef gClientRef
     skillValue <- fmap (^.cvValue) skillCVar
     coopValue <- fmap (^.cvValue) coopCVar
     verifyAndPickupPowerup edictRef edict otherRef gClientRef gClient gItemRef gItem skillValue coopValue

verifyAndPickupPowerup :: Ref' EdictT -> EdictT -> Ref' EdictT -> Ref' GClientT -> GClientT -> Ref' GItemT -> GItemT -> Float -> Float -> Quake Bool
verifyAndPickupPowerup edictRef edict otherRef gClientRef gClient gItemRef gItem skillValue coopValue
  | skillValue == 1 && quantity >= 2 || skillValue >= 2 && quantity >= 1 =
      return False
  | coopValue /= 0 && (gItem^.giFlags) .&. Constants.itStayCoop /= 0 && quantity > 0 =
      return False
  | otherwise = do
      modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) +~ 1)
      applyDeathmatchActions =<< deathmatchValue
      return True
  where quantity = (gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex)
        deathmatchValue = fmap (^.cvValue) deathmatchCVar
        applyDeathmatchActions dmv
          | dmv == 0 = return ()
          | otherwise =
              do when ((edict^.eSpawnFlags) .&. Constants.droppedItem == 0) $
                   setRespawn edictRef (fromIntegral (gItem^.giQuantity))
                 isInstantItem <- checkIfInstantItem  
                 applyInstantItem (isInstantItem || isQuadItem (gItem^.giUse))
        checkIfInstantItem =
          do dmFlagsValue <- fmap (truncate . (^.cvValue)) dmFlagsCVar
             return (dmFlagsValue .&. Constants.dfInstantItems /= 0)
        isQuadItem Nothing = False
        isQuadItem (Just gItemUse) = itemUseId gItemUse == "useQuad" && (edict^.eSpawnFlags) .&. Constants.droppedPlayerItem /= 0
        applyInstantItem False = return ()
        applyInstantItem True =
          do when (isQuadItem (gItem^.giUse)) $ do
               levelTime <- use (gameBaseGlobals.gbLevel.llTime)
               gameItemsGlobals.giQuakeDropTimeoutHack .= truncate (((edict^.eNextThink) - levelTime) / Constants.frameTime)
             maybe itemUseError useThisItem (gItem^.giUse)
        itemUseError = Com.fatalError "GameItems.pickupPowerup gItem^.giUse is Nothing"
        useThisItem f = itemUse f otherRef gItemRef

dropGeneral :: ItemDrop
dropGeneral = ItemDrop "dropGeneral" dropGeneralF

dropGeneralF :: Ref' EdictT -> Ref' GItemT -> Quake ()
dropGeneralF edictRef gItemRef =
  do void (dropItem edictRef gItemRef)
     edict <- readRef edictRef
     gItem <- readRef gItemRef
     proceedDropGeneral edictRef gItem (edict^.eClient)

proceedDropGeneral :: Ref' EdictT -> GItemT -> Maybe (Ref' GClientT) -> Quake ()
proceedDropGeneral _ _ Nothing = Com.fatalError "GameItems.dropGeneral GClientRef is Nothing"
proceedDropGeneral edictRef gItem (Just gClientRef) =
  do modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) -~ 1)
     GameUtil.validateSelectedItem edictRef

useSilencer :: ItemUse
useSilencer = ItemUse "useSilencer" useSilencerF

useSilencerF :: Ref' EdictT -> Ref' GItemT -> Quake ()
useSilencerF edictRef gItemRef =
  do edict <- readRef edictRef
     gItem <- readRef gItemRef
     proceedUseSilencer edictRef gItem (edict^.eClient)

proceedUseSilencer :: Ref' EdictT -> GItemT -> Maybe (Ref' GClientT) -> Quake ()
proceedUseSilencer _ _ Nothing = Com.fatalError "GameItems.useSilencer GClientRef is Nothing"
proceedUseSilencer edictRef gItem (Just gClientRef) =
  do modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) -~ 1)
     GameUtil.validateSelectedItem edictRef
     modifyRef gClientRef (\v -> v & gcSilencerShots +~ 30)
     playItemSound edictRef "items/damage.wav" =<< gameImport
  where gameImport = use (gameBaseGlobals.gbGameImport)
  
useInvulnerability :: ItemUse
useInvulnerability = ItemUse "useInvulnerability" (useGenericItem errMsg calcFrameNum updateFrameNum soundItem)
  where errMsg = "GameItems.proceedUseInvulnerability GClientRef is Nothing"
        calcFrameNum gClient frameNum
          | (gClient^.gcInvincibleFrameNum) > fromIntegral frameNum =
              (gClient^.gcInvincibleFrameNum) + 300
          | otherwise = fromIntegral frameNum + 300
        updateFrameNum frameNum v = v & gcInvincibleFrameNum .~ frameNum
        soundItem = "items/protect.wav"

useBreather :: ItemUse
useBreather = ItemUse "useBreather" (useGenericItem errMsg calcFrameNum updateFrameNum soundItem)
  where errMsg = "GameItems.useBreather GClientRef is Nothing"
        calcFrameNum gClient frameNum
          | (gClient^.gcBreatherFrameNum) > fromIntegral frameNum =
              (gClient^.gcBreatherFrameNum) + 300
          | otherwise = fromIntegral frameNum + 300
        updateFrameNum frameNum v = v & gcBreatherFrameNum .~ frameNum
        soundItem = "items/damage.wav"

useEnviroSuit :: ItemUse
useEnviroSuit = ItemUse "useEnviroSuit" (useGenericItem errMsg calcFrameNum updateFrameNum soundItem)
  where errMsg = "GameItems.useEnviroSuit GClientRef is Nothing"
        calcFrameNum gClient frameNum
          | (gClient^.gcEnviroFrameNum) > fromIntegral frameNum =
              (gClient^.gcEnviroFrameNum) + 300
          | otherwise = fromIntegral frameNum + 300
        updateFrameNum frameNum v = v & gcEnviroFrameNum .~ frameNum
        soundItem = "items/damage.wav"

useGenericItem :: B.ByteString -> (GClientT -> Int -> Float) -> (Float -> GClientT -> GClientT) -> B.ByteString -> Ref' EdictT -> Ref' GItemT -> Quake ()
useGenericItem errMsg calcFrameNum updateFrameNum soundItem edictRef gItemRef =
  do edict <- readRef edictRef
     gItem <- readRef gItemRef
     proceedUseGenericItem errMsg calcFrameNum updateFrameNum soundItem edictRef gItem (edict^.eClient)

proceedUseGenericItem :: B.ByteString -> (GClientT -> Int -> Float) -> (Float -> GClientT -> GClientT) -> B.ByteString -> Ref' EdictT -> GItemT -> Maybe (Ref' GClientT) -> Quake ()
proceedUseGenericItem errMsg _ _ _ _ _ Nothing = Com.fatalError errMsg
proceedUseGenericItem _ calcFrameNum updateFrameNum soundItem edictRef gItem (Just gClientRef) =
  do modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) -~ 1)
     GameUtil.validateSelectedItem edictRef
     gClient <- readRef gClientRef
     genericItemFrameNum <- calcFrameNum gClient <$> levelFrameNum
     modifyRef gClientRef (updateFrameNum genericItemFrameNum)
     playItemSound edictRef soundItem =<< gameImport
  where levelFrameNum = use (gameBaseGlobals.gbLevel.llFrameNum)
        gameImport = use (gameBaseGlobals.gbGameImport)

pickupAncientHead :: EntInteract
pickupAncientHead = EntInteract "pickupAncientHead" pickupAncientHeadF

pickupAncientHeadF :: Ref' EdictT -> Ref' EdictT -> Quake Bool
pickupAncientHeadF edictRef otherRef =
  do modifyRef otherRef (\v -> v & eMaxHealth +~ 2)
     commonDeathmatchActions gItemError edictRef =<< deathmatchValue
     return True
  where deathmatchValue = fmap (^.cvValue) deathmatchCVar
        gItemError = "GameItems.pickupAncientHead GItemRef is Nothing"

pickupAdrenaline :: EntInteract
pickupAdrenaline = EntInteract "pickupAdrenaline" pickupAdrenalineF

pickupAdrenalineF :: Ref' EdictT -> Ref' EdictT -> Quake Bool
pickupAdrenalineF edictRef otherRef =
  do dmv <- fmap (^.cvValue) deathmatchCVar
     updateMaxHealth dmv
     updateHealth =<< readRef otherRef
     commonDeathmatchActions gItemError edictRef dmv
     return True
  where updateMaxHealth dmv
          | dmv == 0 = modifyRef otherRef (\v -> v & eMaxHealth +~ 1)
          | otherwise = return ()
        updateHealth other
          | (other^.eHealth) < (other^.eMaxHealth) =
              modifyRef otherRef (\v -> v & eHealth .~ (other^.eMaxHealth))
          | otherwise = return ()
        gItemError = "GameItems.pickupAdrenaline GItemRef is Nothing"

pickupBandolier :: EntInteract
pickupBandolier = EntInteract "pickupBandolier" pickupBandolierF

pickupBandolierF :: Ref' EdictT -> Ref' EdictT -> Quake Bool
pickupBandolierF = pickupGeneric gItemError checkAmmoAndStuff
  where gClientError = "GameItems.pickupBandolier GClientRef is Nothing"
        gItemError = "GameItems.pickupBandolier GItemRef is Nothing"
        checkAmmoAndStuff Nothing = Com.fatalError gClientError
        checkAmmoAndStuff (Just gClientRef) =
          checkCommonAmmoAndStuff checkMaxAmmo gClientRef
        checkMaxAmmo gClientRef =
          modifyRef gClientRef (\v -> v & gcPers.cpMaxBullets %~ setLimit 250
                                        & gcPers.cpMaxShells %~ setLimit 150
                                        & gcPers.cpMaxCells %~ setLimit 250
                                        & gcPers.cpMaxSlugs %~ setLimit 75)

pickupPack :: EntInteract
pickupPack = EntInteract "pickupPack" pickupPackF

pickupPackF :: Ref' EdictT -> Ref' EdictT -> Quake Bool
pickupPackF = pickupGeneric gItemError checkAmmoAndStuff
  where gClientError = "GameItems.pickupPack GClientRef is Nothing"
        gItemError = "GameItems.pickupPack GItemRef is Nothing"
        checkAmmoAndStuff Nothing = Com.fatalError gClientError
        checkAmmoAndStuff (Just gClientRef) =
          do checkCommonAmmoAndStuff checkMaxAmmo gClientRef
             checkCells gClientRef
             checkGrenades gClientRef
             checkRockets gClientRef
             checkSlugs gClientRef
        checkMaxAmmo gClientRef =
          modifyRef gClientRef (\v -> v & gcPers.cpMaxBullets %~ setLimit 300
                                        & gcPers.cpMaxShells %~ setLimit 200
                                        & gcPers.cpMaxRockets %~ setLimit 100
                                        & gcPers.cpMaxGrenades %~ setLimit 100
                                        & gcPers.cpMaxCells %~ setLimit 300
                                        & gcPers.cpMaxSlugs %~ setLimit 100)

pickupGeneric :: B.ByteString -> (Maybe (Ref' GClientT) -> Quake ()) -> Ref' EdictT -> Ref' EdictT -> Quake Bool
pickupGeneric gItemError checkAmmoAndStuff edictRef otherRef =
  do other <- readRef otherRef
     checkAmmoAndStuff (other^.eClient)
     commonDeathmatchActions gItemError edictRef =<< deathmatchValue
     return True
  where deathmatchValue = fmap (^.cvValue) deathmatchCVar

setLimit :: Int -> Int -> Int
setLimit limit v | v < limit = limit
                 | otherwise = v

checkCommonAmmoAndStuff :: (Ref' GClientT -> Quake ()) -> Ref' GClientT -> Quake ()
checkCommonAmmoAndStuff checkMaxAmmo gClientRef =
  do checkMaxAmmo gClientRef
     checkBullets gClientRef
     checkShells gClientRef

commonDeathmatchActions :: B.ByteString -> Ref' EdictT -> Float -> Quake ()
commonDeathmatchActions gItemError edictRef dmv
  | dmv == 0 = return ()
  | otherwise = applyActions =<< readRef edictRef
  where applyActions edict
          | (edict^.eSpawnFlags) .&. Constants.droppedItem == 0 =
              respawnItem gItemError edictRef (edict^.eItem)
          | otherwise = return ()

pickupKey :: EntInteract
pickupKey = EntInteract "pickupKey" pickupKeyF

pickupKeyF :: Ref' EdictT -> Ref' EdictT -> Quake Bool
pickupKeyF edictRef otherRef =
  do edict <- readRef edictRef
     other <- readRef otherRef
     proceedPickupKey edict (other^.eClient) (edict^.eItem) =<< coopValue
  where coopValue = fmap (^.cvValue) coopCVar

proceedPickupKey :: EdictT -> Maybe (Ref' GClientT) -> Maybe (Ref' GItemT) -> Float -> Quake Bool
proceedPickupKey  _ Nothing _ _ = pickupError "GameItems.pickupKey GClientRef is Nothing"
proceedPickupKey  _ _ Nothing _ = pickupError "GameItems.pickupKey GItemRef is Nothing"
proceedPickupKey edict (Just gClientRef) (Just gItemRef) coopValue
  | coopValue /= 0 =
      do gItem <- readRef gItemRef
         gClient <- readRef gClientRef
         pickupKeyCoop edict gClientRef gClient gItem
  | otherwise =
      do gItem <- readRef gItemRef
         modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) .~ 1)
         return True

pickupKeyCoop :: EdictT -> Ref' GClientT -> GClientT -> GItemT -> Quake Bool
pickupKeyCoop edict gClientRef gClient gItem
  | isPowerCubeKey && possessIt = return False
  | isPowerCubeKey =
      do modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) +~ 1
                                       & gcPers.cpPowerCubes %~ (.|. (((edict^.eSpawnFlags) .&. 0xFF00) `shiftR` 8)))
         return True
  | haveThatKey = return False
  | otherwise =
      do modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) .~ 1)
         return True
  where isPowerCubeKey = (edict^.eClassName) == "key_power_cube"
        possessIt = (gClient^.gcPers.cpPowerCubes) .&. (((edict^.eSpawnFlags) .&. 0xFF00) `shiftR` 8) /= 0
        haveThatKey = (gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex) /= 0

pickupHealth :: EntInteract
pickupHealth = EntInteract "pickupHealth" pickupHealthF

pickupHealthF :: Ref' EdictT -> Ref' EdictT -> Quake Bool
pickupHealthF edictRef otherRef =
  do edict <- readRef edictRef
     other <- readRef otherRef
     proceedPickupHealth edictRef edict otherRef other

proceedPickupHealth :: Ref' EdictT -> EdictT -> Ref' EdictT -> EdictT -> Quake Bool
proceedPickupHealth edictRef edict otherRef other
  | noNeedInHealth = return False
  | otherwise =
      do modifyRef otherRef (\v -> v & eHealth +~ (edict^.eCount))
         when noHealthIgnoreMax $
           modifyRef otherRef (\v -> v & eHealth %~ resetToMaxHealth)
         checkTimedHealth
         return True
  where noNeedInHealth = noHealthIgnoreMax && alreadyAtFull
        noHealthIgnoreMax = (edict^.eStyle) .&. Constants.healthIgnoreMax == 0
        alreadyAtFull = (other^.eHealth) >= (other^.eMaxHealth)
        resetToMaxHealth v | v > (other^.eMaxHealth) = other^.eMaxHealth
                           | otherwise = v
        checkTimedHealth
          | (edict^.eStyle) .&. Constants.healthTimed /= 0 =
              do levelTime <- use (gameBaseGlobals.gbLevel.llTime)
                 modifyRef edictRef (\v -> v & eThink .~ Just GameUtil.megaHealthThink
                                             & eNextThink .~ levelTime + 5
                                             & eOwner .~ Just otherRef
                                             & eFlags %~ (.|. Constants.flRespawn)
                                             & eSvFlags %~ (.|. Constants.svfNoClient)
                                             & eSolid .~ Constants.solidNot)
          | otherwise =
              applyDeathmatchActions =<< deathmatchValue
        deathmatchValue = fmap (^.cvValue) deathmatchCVar
        applyDeathmatchActions dmv
          | (edict^.eSpawnFlags) .&. Constants.droppedItem /= 0 && dmv /= 0 =
              setRespawn edictRef 30
          | otherwise = return ()

findItem :: B.ByteString -> Quake (Maybe (Ref' GItemT))
findItem pickupName =
  do items <- use (gameBaseGlobals.gbItemList)
     case search items of
       Nothing ->
         do Com.printf (B.concat ["Item not found:", pickupName, "\n"])
            return Nothing
       Just idx -> return (Just (Ref idx))
  where search items = V.findIndex searchByName (V.drop 1 items)
        name = BC.map toLower pickupName
        searchByName gItem
          | name == BC.map toLower (fromMaybe B.empty (gItem^.giPickupName)) = True
          | otherwise = False

setRespawn :: Ref' EdictT -> Float -> Quake ()
setRespawn edictRef delay =
  do levelTime <- use (gameBaseGlobals.gbLevel.llTime)
     modifyRef edictRef (\v -> v & eFlags %~ (.|. Constants.flRespawn)
                                 & eSvFlags %~ (.|. Constants.svfNoClient)
                                 & eSolid .~ Constants.solidNot
                                 & eNextThink .~ levelTime + delay
                                 & eThink .~ Just doRespawn)
     linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
     linkEntity edictRef

armorIndex :: Ref' EdictT -> Quake Int
armorIndex edictRef =
  do edict <- readRef edictRef
     getArmorIndex (edict^.eClient)
  where getArmorIndex Nothing = return 0
        getArmorIndex (Just gClientRef) =
          do gClient <- readRef gClientRef
             jacketArmor <- readRef =<< use (gameItemsGlobals.giJacketArmorIndex)
             combatArmor <- readRef =<< use (gameItemsGlobals.giCombatArmorIndex)
             bodyArmor <- readRef =<< use (gameItemsGlobals.giBodyArmorIndex)
             return (pickArmorIndex gClient jacketArmor combatArmor bodyArmor)

pickArmorIndex :: GClientT -> GItemT -> GItemT -> GItemT -> Int
pickArmorIndex gClient jacketArmor combatArmor bodyArmor
  | inventory UV.! (jacketArmor^.giIndex) > 0 = jacketArmor^.giIndex
  | inventory UV.! (combatArmor^.giIndex) > 0 = combatArmor^.giIndex
  | inventory UV.! (bodyArmor^.giIndex) > 0 = bodyArmor^.giIndex
  | otherwise = 0
  where inventory = gClient^.gcPers.cpInventory

addAmmo :: Ref' EdictT -> Ref' GItemT -> Int -> Quake Bool
addAmmo edictRef gItemRef count =
  do edict <- readRef edictRef
     addPlayerAmmo (edict^.eClient)
  where addPlayerAmmo Nothing = return False
        addPlayerAmmo (Just gClientRef) =
          do gClient <- readRef gClientRef
             gItem <- readRef gItemRef
             maybe (return False) (applyPlayerAmmo gClientRef gClient gItem) (chooseMaximum gClient gItem)
        chooseMaximum gClient gItem
          | (gItem^.giTag) == Constants.ammoBullets = Just (gClient^.gcPers.cpMaxBullets)
          | (gItem^.giTag) == Constants.ammoShells = Just (gClient^.gcPers.cpMaxShells)
          | (gItem^.giTag) == Constants.ammoRockets = Just (gClient^.gcPers.cpMaxRockets)
          | (gItem^.giTag) == Constants.ammoGrenades = Just (gClient^.gcPers.cpMaxGrenades)
          | (gItem^.giTag) == Constants.ammoCells = Just (gClient^.gcPers.cpMaxCells)
          | (gItem^.giTag) == Constants.ammoSlugs = Just (gClient^.gcPers.cpMaxSlugs)
          | otherwise = Nothing
        applyPlayerAmmo gClientRef gClient gItem maxAmmo
          | (gClient^.gcPers.cpInventory) UV.! (gItem^.giIndex) == maxAmmo = return False
          | otherwise =
              do modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) %~ updateAmmo maxAmmo)
                 return True
        updateAmmo maxAmmo v = min (v + count) maxAmmo

dropItem :: Ref' EdictT -> Ref' GItemT -> Quake (Ref' EdictT)
dropItem edictRef gItemRef =
  do droppedRef <- GameUtil.spawn
     edict <- readRef edictRef
     gItem <- readRef gItemRef
     modifyRef droppedRef (\v -> v & eClassName .~ (gItem^.giClassName)
                                   & eItem .~ Just gItemRef
                                   & eSpawnFlags .~ Constants.droppedItem
                                   & eEntityState.esEffects .~ (gItem^.giWorldModelFlags)
                                   & eEntityState.esRenderFx .~ Constants.rfGlow
                                   & eMins .~ V3 (-15) (-15) (-15)
                                   & eMaxs .~ V3 15 15 15
                                   & eSolid .~ Constants.solidTrigger
                                   & eMoveType .~ Constants.moveTypeToss
                                   & eTouch .~ Just dropTempTouch
                                   & eOwner .~ Just edictRef)
     gameImport <- use (gameBaseGlobals.gbGameImport)
     (gameImport^.giSetModel) droppedRef (gItem^.giWorldModel)
     forward <- maybe (calcNoClientForward droppedRef edict) 
                      (calcClientForward droppedRef edictRef edict)
                      (edict^.eClient)
     levelTime <- use (gameBaseGlobals.gbLevel.llTime)
     modifyRef droppedRef (\v -> v & eVelocity .~ (fmap (* 100) forward & _z .~ 300)
                                   & eThink .~ Just dropMakeTouchable
                                   & eNextThink .~ levelTime + 1)
     (gameImport^.giLinkEntity) droppedRef
     return droppedRef

calcNoClientForward :: Ref' EdictT -> EdictT -> Quake (V3 Float)
calcNoClientForward droppedRef edict =
  do modifyRef droppedRef (\v -> v & eEntityState.esOrigin .~ (edict^.eEntityState.esOrigin))
     return forward
  where (forward, _, _) = Math3D.angleVectors (edict^.eEntityState.esAngles) True False False

calcClientForward :: Ref' EdictT -> Ref' EdictT -> EdictT -> Ref' GClientT -> Quake (V3 Float)
calcClientForward droppedRef edictRef edict gClientRef =
  do gClient <- readRef gClientRef
     let (forward, right, _) = Math3D.angleVectors (gClient^.gcVAngle) True True False
         offset = V3 24 0 (-16)
         origin = Math3D.projectSource (edict^.eEntityState.esOrigin) offset forward right
     modifyRef droppedRef (\v -> v & eEntityState.esOrigin .~ origin)
     dropped <- readRef droppedRef
     trace <- use (gameBaseGlobals.gbGameImport.giTrace)
     traceT <- trace (edict^.eEntityState.esOrigin) (Just (dropped^.eMins)) (Just (dropped^.eMaxs)) (dropped^.eEntityState.esOrigin) (Just edictRef) Constants.contentsSolid
     modifyRef droppedRef (\v -> v & eEntityState.esOrigin .~ (traceT^.tEndPos))
     return forward

playItemSound :: Ref' EdictT -> B.ByteString -> GameImportT -> Quake ()
playItemSound edictRef soundItem gameImport =
  do soundIdx <- soundIndex (Just soundItem)
     sound (Just edictRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0
  where soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound
        
respawnItem :: B.ByteString -> Ref' EdictT -> Maybe (Ref' GItemT) -> Quake ()
respawnItem errMsg _ Nothing = Com.fatalError errMsg
respawnItem _ edictRef (Just gItemRef) =
  do gItem <- readRef gItemRef
     setRespawn edictRef (fromIntegral (gItem^.giQuantity))

checkBullets :: Ref' GClientT -> Quake ()
checkBullets = checkAmmoGeneric "Bullets" itemMaxQuantity
  where itemMaxQuantity gClient = gClient^.gcPers.cpMaxBullets

checkShells :: Ref' GClientT -> Quake ()
checkShells = checkAmmoGeneric "Shells" itemMaxQuantity
  where itemMaxQuantity gClient = gClient^.gcPers.cpMaxShells

checkCells :: Ref' GClientT -> Quake ()
checkCells = checkAmmoGeneric "Cells" itemMaxQuantity
  where itemMaxQuantity gClient = gClient^.gcPers.cpMaxCells

checkGrenades :: Ref' GClientT -> Quake ()
checkGrenades = checkAmmoGeneric "Grenades" itemMaxQuantity
  where itemMaxQuantity gClient = gClient^.gcPers.cpMaxGrenades

checkRockets :: Ref' GClientT -> Quake ()
checkRockets = checkAmmoGeneric "Rockets" itemMaxQuantity
  where itemMaxQuantity gClient = gClient^.gcPers.cpMaxRockets

checkSlugs :: Ref' GClientT -> Quake ()
checkSlugs = checkAmmoGeneric "Slugs" itemMaxQuantity
  where itemMaxQuantity gClient = gClient^.gcPers.cpMaxSlugs

checkAmmoGeneric :: B.ByteString -> (GClientT -> Int) -> Ref' GClientT -> Quake ()
checkAmmoGeneric itemName itemMaxQuantity gClientRef =
  do foundItem <- findItem itemName
     maybe (return ()) applyItem foundItem
  where applyItem gItemRef =
          do gClient <- readRef gClientRef
             gItem <- readRef gItemRef
             modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) %~ updateQuantity gClient gItem)
        updateQuantity gClient gItem v = min (v + (gItem^.giQuantity)) (itemMaxQuantity gClient)

doRespawn :: EntThink
doRespawn = EntThink "doRespawn" doRespawnF

doRespawnF :: Ref' EdictT -> Quake Bool
doRespawnF edictRef =
  do edict <- readRef edictRef
     entRef <- maybe (return edictRef) (pickRandomTeamMember edict) (edict^.eTeam)
     modifyRef entRef (\v -> v & eSvFlags %~ (.&. complement Constants.svfNoClient)
                               & eSolid .~ Constants.solidTrigger)
     linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
     linkEntity entRef
     modifyRef entRef (\v -> v & eEntityState.esEvent .~ Constants.evItemRespawn)
     return False
  where pickRandomTeamMember edict _ = 
          do count <- countTeamDepth (edict^.eTeamMaster) 0
             choice <- fmap ((`mod` count) . fromIntegral) Lib.rand :: Quake Int
             chooseTeamMember (edict^.eTeamMaster) 0 choice
        countTeamDepth Nothing count = return count
        countTeamDepth (Just entRef) count =
          do ent <- readRef entRef
             countTeamDepth (ent^.eChain) (count + 1)
        chooseTeamMember Nothing _ _ = chooseError
        chooseTeamMember (Just entRef) idx maxIdx
          | idx >= maxIdx = return entRef
          | otherwise =
              do ent <- readRef entRef
                 chooseTeamMember (ent^.eChain) (idx + 1) maxIdx
        chooseError =
          do Com.fatalError "GameItems.doRespawn ent^.eTeamMaster/ent^.eChain is Nothing"
             return (Ref (-1))

dropTempTouch :: EntTouch
dropTempTouch = EntTouch "dropTempTouch" dropTempTouchF

dropTempTouchF :: Ref' EdictT -> Ref' EdictT -> CPlaneT -> Maybe CSurfaceT -> Quake ()
dropTempTouchF edictRef otherRef plane surf =
  do edict <- readRef edictRef
     unless (Just otherRef == (edict^.eOwner)) $
       entTouch touchItem edictRef otherRef plane surf

dropMakeTouchable :: EntThink
dropMakeTouchable = EntThink "dropMakeTouchable" dropMakeTouchableF

dropMakeTouchableF :: Ref' EdictT -> Quake Bool
dropMakeTouchableF edictRef =
  do modifyRef edictRef (\v -> v & eTouch .~ Just touchItem)
     applyDeathmatchActions =<< deathmatchValue
     return False
  where deathmatchValue = fmap (^.cvValue) deathmatchCVar
        applyDeathmatchActions dmv
          | dmv == 0 = return ()
          | otherwise =
              do levelTime <- use (gameBaseGlobals.gbLevel.llTime)
                 modifyRef edictRef (\v -> v & eNextThink .~ levelTime + 29
                                             & eThink .~ Just GameUtil.freeEdictA)

touchItem :: EntTouch
touchItem = error "GameItems.touchItem" -- TODO

spawnItem :: Ref' EdictT -> Ref' GItemT -> Quake ()
spawnItem = error "GameItems.spawnItem" -- TODO

spItemHealth :: Ref' EdictT -> Quake ()
spItemHealth = error "GameItems.spItemHealth" -- TODO

spItemHealthLarge :: Ref' EdictT -> Quake ()
spItemHealthLarge = error "GameItems.spItemHealthLarge" -- TODO

spItemHealthMega :: Ref' EdictT -> Quake ()
spItemHealthMega = error "GameItems.spItemHealthMega" -- TODO

spItemHealthSmall :: Ref' EdictT -> Quake ()
spItemHealthSmall = error "GameItems.spItemHealthSmall" -- TODO

initItems :: Quake ()
initItems =
  gameBaseGlobals.gbGame.glNumItems .= V.length GameItemsList.itemList -- TODO; jake2 has -1 here...
