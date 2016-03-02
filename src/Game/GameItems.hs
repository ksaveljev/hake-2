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
import qualified Game.GameUtil as GameUtil
import qualified QCommon.Com as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types

import           Control.Lens (use, ix, (^.), (.=), (&), (.~), (+~), (-~), (%~))
import           Control.Monad (when, void)
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
pickupAmmo = EntInteract "pickupAmmo" pickupAmmoF

pickupAmmoF :: EdictRef -> EdictRef -> Quake Bool
pickupAmmoF edictRef otherRef =
  do edict <- readRef edictRef
     other <- readRef otherRef
     proceedPickupAmmo edictRef edict otherRef (other^.eClient) (edict^.eItem)

proceedPickupAmmo :: EdictRef -> EdictT -> EdictRef -> Maybe GClientRef -> Maybe GItemRef -> Quake Bool
proceedPickupAmmo _ _ _ Nothing _ = pickupError "GameItems.pickupAmmo GClientRef is Nothing"
proceedPickupAmmo _ _ _ _ Nothing = pickupError "GameItems.pickupAmmo GItemRef is Nothing"
proceedPickupAmmo edictRef edict otherRef (Just gClientRef) (Just gItemRef) =
  do gItem <- readRef gItemRef
     gClient <- readRef gClientRef
     calcAndAddAmmo edictRef edict otherRef gClientRef gClient gItemRef gItem =<< dmFlagsValue
  where dmFlagsValue = fmap (truncate . (^.cvValue)) dmFlagsCVar

calcAndAddAmmo :: EdictRef -> EdictT -> EdictRef -> GClientRef -> GClientT -> GItemRef -> GItemT -> Int -> Quake Bool
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

dropAmmoF :: EdictRef -> GItemRef -> Quake ()
dropAmmoF edictRef gItemRef =
  do gItem <- readRef gItemRef
     droppedEdictRef <- dropItem edictRef gItemRef
     edict <- readRef edictRef
     proceedDropAmmo edictRef gItem droppedEdictRef (edict^.eClient)

proceedDropAmmo :: EdictRef -> GItemT -> EdictRef -> Maybe GClientRef -> Quake ()
proceedDropAmmo _ _ _ Nothing = Com.fatalError "GameItems.dropAmmo GClientRef is Nothing"
proceedDropAmmo edictRef gItem droppedEdictRef (Just gClientRef) =
  readRef gClientRef >>= calcAndDropAmmo edictRef gItem droppedEdictRef gClientRef

calcAndDropAmmo :: EdictRef -> GItemT -> EdictRef -> GClientRef -> GClientT -> Quake ()
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
        checkIfCanDropWeapon :: EdictT -> GItemRef -> Quake ()
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

useQuadF :: EdictRef -> GItemRef -> Quake ()
useQuadF edictRef gItemRef =
  do edict <- readRef edictRef
     gItem <- readRef gItemRef
     proceedUseQuad edictRef gItem (edict^.eClient)

proceedUseQuad :: EdictRef -> GItemT -> Maybe GClientRef -> Quake ()
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

pickupPowerupF :: EdictRef -> EdictRef -> Quake Bool
pickupPowerupF edictRef otherRef =
  do edict <- readRef edictRef
     other <- readRef otherRef
     proceedPickupPowerup edictRef edict otherRef (other^.eClient) (edict^.eItem)

proceedPickupPowerup :: EdictRef -> EdictT -> EdictRef -> Maybe GClientRef -> Maybe GItemRef -> Quake Bool
proceedPickupPowerup _ _ _ Nothing _ = pickupError "GameItems.pickupPowerup GClientRef is Nothing"
proceedPickupPowerup _ _ _ _ Nothing = pickupError "GameItems.pickupPowerup GItemRef is Nothing"
proceedPickupPowerup edictRef edict otherRef (Just gClientRef) (Just gItemRef) =
  do gItem <- readRef gItemRef
     gClient <- readRef gClientRef
     skillValue <- fmap (^.cvValue) skillCVar
     coopValue <- fmap (^.cvValue) coopCVar
     verifyAndPickupPowerup edictRef edict otherRef gClientRef gClient gItemRef gItem skillValue coopValue

verifyAndPickupPowerup :: EdictRef -> EdictT -> EdictRef -> GClientRef -> GClientT -> GItemRef -> GItemT -> Float -> Float -> Quake Bool
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
        applyInstantItem :: Bool -> Quake ()
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

dropGeneralF :: EdictRef -> GItemRef -> Quake ()
dropGeneralF edictRef gItemRef =
  do void (dropItem edictRef gItemRef)
     edict <- readRef edictRef
     gItem <- readRef gItemRef
     proceedDropGeneral edictRef gItem (edict^.eClient)

proceedDropGeneral :: EdictRef -> GItemT -> Maybe GClientRef -> Quake ()
proceedDropGeneral _ _ Nothing = Com.fatalError "GameItems.dropGeneral GClientRef is Nothing"
proceedDropGeneral edictRef gItem (Just gClientRef) =
  do modifyRef gClientRef (\v -> v & gcPers.cpInventory.ix (gItem^.giIndex) -~ 1)
     GameUtil.validateSelectedItem edictRef

useSilencer :: ItemUse
useSilencer = ItemUse "useSilencer" useSilencerF

useSilencerF :: EdictRef -> GItemRef -> Quake ()
useSilencerF edictRef gItemRef =
  do edict <- readRef edictRef
     gItem <- readRef gItemRef
     proceedUseSilencer edictRef gItem (edict^.eClient)

proceedUseSilencer :: EdictRef -> GItemT -> Maybe GClientRef -> Quake ()
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

useGenericItem :: B.ByteString -> (GClientT -> Int -> Float) -> (Float -> GClientT -> GClientT) -> B.ByteString -> EdictRef -> GItemRef -> Quake ()
useGenericItem errMsg calcFrameNum updateFrameNum soundItem edictRef gItemRef =
  do edict <- readRef edictRef
     gItem <- readRef gItemRef
     proceedUseGenericItem errMsg calcFrameNum updateFrameNum soundItem edictRef gItem (edict^.eClient)

proceedUseGenericItem :: B.ByteString -> (GClientT -> Int -> Float) -> (Float -> GClientT -> GClientT) -> B.ByteString -> EdictRef -> GItemT -> Maybe GClientRef -> Quake ()
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

pickupAncientHeadF :: EdictRef -> EdictRef -> Quake Bool
pickupAncientHeadF edictRef otherRef =
  do modifyRef otherRef (\v -> v & eMaxHealth +~ 2)
     edict <- readRef edictRef
     applyDeathmatchActions edict =<< deathmatchValue
     return True
  where deathmatchValue = fmap (^.cvValue) deathmatchCVar
        applyDeathmatchActions edict dmv
          | (edict^.eSpawnFlags) .&. Constants.droppedItem == 0 && dmv /= 0 =
              respawnItem (edict^.eItem)
          | otherwise = return ()
        respawnItem :: Maybe GItemRef -> Quake ()
        respawnItem Nothing = Com.fatalError "GameItems.pickupAncientHead edict^.eItem is Nothing"
        respawnItem (Just gItemRef) =
          do gItem <- readRef gItemRef
             setRespawn edictRef (fromIntegral (gItem^.giQuantity))

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

addAmmo :: EdictRef -> GItemRef -> Int -> Quake Bool
addAmmo = error "GameItems.addAmmo" -- TODO

dropItem :: EdictRef -> GItemRef -> Quake EdictRef
dropItem = error "GameItems.dropItem" -- TODO
          
playItemSound :: EdictRef -> B.ByteString -> GameImportT -> Quake ()
playItemSound edictRef soundItem gameImport =
  do soundIdx <- soundIndex (Just soundItem)
     sound (Just edictRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0
  where soundIndex = gameImport^.giSoundIndex
        sound = gameImport^.giSound