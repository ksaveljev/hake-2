module Game.PlayerHud
    ( checkChaseStats
    , deathmatchScoreboardMessage
    , moveClientToIntermission
    , setSpectatorStats
    , setStats
    ) where

import           Control.Lens (use, ix, (^.), (&), (.~), (%~))
import           Control.Monad (when)
import           Data.Bits (complement, (.&.), (.|.))
import           Data.Int (Int16)
import           Data.Maybe (isJust)
import qualified Data.Vector.Unboxed as UV

import qualified Constants
import           Game.ClientPersistantT
import           Game.ClientRespawnT
import           Game.CVarT
import           Game.EdictT
import           Game.GClientT
import qualified Game.GameItems as GameItems
import           Game.GItemT
import           Game.LevelLocalsT
import           Game.PlayerStateT
import qualified QCommon.Com as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types

checkChaseStats :: Ref EdictT -> Quake ()
checkChaseStats edictRef = do
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    setChaseStats 1 maxClients

  where
    setChaseStats idx maxIdx
        | idx > maxIdx = return ()
        | otherwise = do
            newEdict <- readRef (Ref idx)
            gClientRef <- getGClientRef (newEdict^.eClient)
            gClient <- readRef gClientRef
            if not (newEdict^.eInUse) || (gClient^.gcChaseTarget) /= Just edictRef
                then
                    setChaseStats (idx + 1) maxIdx
                else do
                    edict <- readRef edictRef
                    edictClientRef <- getGClientRef (edict^.eClient)
                    edictClient <- readRef edictClientRef
                    modifyRef gClientRef (\v -> v & gcPlayerState.psStats .~ (edictClient^.gcPlayerState.psStats))
                    setSpectatorStats (Ref idx)
                    setChaseStats (idx + 1) maxIdx
    getGClientRef Nothing = do
        Com.fatalError "PlayerHud.checkChaseStats#setChaseStats eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef

deathmatchScoreboardMessage :: Ref EdictT -> Maybe (Ref EdictT) -> Quake ()
deathmatchScoreboardMessage = error "PlayerHud.deathmatchScoreboardMessage" -- TODO

moveClientToIntermission :: Ref EdictT -> Quake ()
moveClientToIntermission = error "PlayerHud.moveClientToIntermission" -- TODO

setSpectatorStats :: Ref EdictT -> Quake ()
setSpectatorStats = error "PlayerHud.setSpectatorStats" -- TODO

setStats :: Ref EdictT -> Quake ()
setStats edictRef = do
    edict <- readRef edictRef
    gClientRef <- getGClientRef (edict^.eClient)
    setHealth gClientRef
    setAmmo gClientRef
    setArmor gClientRef
    setPickupMessage gClientRef
    setTimers gClientRef
    setSelectedItem gClientRef
    setLayouts gClientRef
    setFrags gClientRef
    setHelpIcon gClientRef
    modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statSpectator .~ 0)
  where
    getGClientRef Nothing = do
        Com.fatalError "PlayerHud.setStats edict^.eClient is Nothing"
        return (Ref (-1))
    getGClientRef (Just gClientRef) = return gClientRef
    setHealth gClientRef = do
        edict <- readRef edictRef
        picHealth <- use (gameBaseGlobals.gbLevel.llPicHealth)
        modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statHealthIcon .~ fromIntegral picHealth
                                      & gcPlayerState.psStats.ix Constants.statHealth .~ fromIntegral (edict^.eHealth))
    setAmmo gClientRef = do
        gClient <- readRef gClientRef
        doSetAmmo gClientRef gClient
    doSetAmmo gClientRef gClient
        | (gClient^.gcAmmoIndex) == 0 =
            modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statAmmoIcon .~ 0
                                          & gcPlayerState.psStats.ix Constants.statAmmo .~ 0)
        | otherwise = do
            item <- readRef (Ref (gClient^.gcAmmoIndex))
            imageIndex <- use (gameBaseGlobals.gbGameImport.giImageIndex)
            imageIdx <- imageIndex (item^.giIcon)
            modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statAmmoIcon .~ fromIntegral imageIdx
                                          & gcPlayerState.psStats.ix Constants.statAmmo .~ fromIntegral ((gClient^.gcPers.cpInventory) UV.! (gClient^.gcAmmoIndex)))
    setArmor gClientRef = do
        powerArmorType <- GameItems.powerArmorType edictRef
        gameImport <- use (gameBaseGlobals.gbGameImport)
        -- TODO: old code, needs refactoring
        (powerArmorType', cells) <-
          if powerArmorType /= 0
            then do
              Just gItemRef <- GameItems.findItem "cells" -- TODO
              item <- readRef gItemRef
              gClient <- readRef gClientRef
              let cells = (gClient^.gcPers.cpInventory) UV.! (item^.giIndex)
              if cells == 0 -- ran out of cells for power armor
                then do
                  modifyRef edictRef (\v -> v & eFlags %~ (.&. (complement Constants.flPowerArmor)))
                  soundIdx <- (gameImport^.giSoundIndex) (Just "misc/power2.wav")
                  (gameImport^.giSound) (Just edictRef) Constants.chanItem soundIdx 1 Constants.attnNorm 0
                  return (0, cells)
                else
                  return (powerArmorType, cells)
            else
              return (0, 0)
        armorIdx <- GameItems.armorIndex edictRef
        frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
        gClient <- readRef gClientRef
        (icon, armor) <- getArmorWithIcon gClient powerArmorType' frameNum armorIdx cells
        modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statArmorIcon .~ fromIntegral icon
                                      & gcPlayerState.psStats.ix Constants.statArmor .~ fromIntegral armor)
    getArmorWithIcon gClient powerArmorType frameNum armorIdx cells
        | powerArmorType /= 0 && (armorIdx == 0 || frameNum .&. 8 /= 0) = do -- flash between power armor and other armor icon
            imageIndex <- use (gameBaseGlobals.gbGameImport.giImageIndex)
            imageIdx <- imageIndex (Just "i_powershield")
            return (imageIdx, cells)
        | armorIdx /= 0 = do
            Just item <- GameItems.getItemByIndex armorIdx
            imageIndex <- use (gameBaseGlobals.gbGameImport.giImageIndex)
            imageIdx <- imageIndex (item^.giIcon)
            return (imageIdx, fromIntegral ((gClient^.gcPers.cpInventory) UV.! armorIdx))
        | otherwise = return (0, 0)
    
    setPickupMessage gClientRef = do
        gClient <- readRef gClientRef
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        when (levelTime > (gClient^.gcPickupMsgTime)) $
            modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statPickupIcon .~ 0
                                          & gcPlayerState.psStats.ix Constants.statPickupString .~ 0)
    setTimers gClientRef = do
        gClient <- readRef gClientRef
        imageIndex <- use (gameBaseGlobals.gbGameImport.giImageIndex)
        frameNum <- fmap fromIntegral (use (gameBaseGlobals.gbLevel.llFrameNum))
        (icon, timer) <- getTimerWithIcon gClient frameNum imageIndex
        modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statTimerIcon .~ fromIntegral icon
                                      & gcPlayerState.psStats.ix Constants.statTimer .~ timer)
    getTimerWithIcon gClient frameNum imageIndex
        | (gClient^.gcQuadFrameNum) > frameNum = do
            imageIdx <- imageIndex (Just "p_quad")
            return (imageIdx, truncate (((gClient^.gcQuadFrameNum) - frameNum) / 10))
        | (gClient^.gcInvincibleFrameNum) > frameNum = do
            imageIdx <- imageIndex (Just "p_invulnerability")
            return (imageIdx, truncate (((gClient^.gcInvincibleFrameNum) - frameNum) / 10))
        | (gClient^.gcEnviroFrameNum) > frameNum = do
            imageIdx <- imageIndex (Just "p_envirosuit")
            return (imageIdx, truncate (((gClient^.gcEnviroFrameNum) - frameNum) / 10))
        | (gClient^.gcBreatherFrameNum) > frameNum = do
            imageIdx <- imageIndex (Just "p_rebreather")
            return (imageIdx, truncate (((gClient^.gcBreatherFrameNum) - frameNum) / 10))
        | otherwise = return (0, 0)
    setSelectedItem gClientRef = do
        gClient <- readRef gClientRef
        imageIndex <- use (gameBaseGlobals.gbGameImport.giImageIndex)
        icon <- getSelectedItemIcon gClient imageIndex
        modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statSelectedIcon .~ fromIntegral icon
                                      & gcPlayerState.psStats.ix Constants.statSelectedItem .~ fromIntegral (gClient^.gcPers.cpSelectedItem))
    getSelectedItemIcon gClient imageIndex
        | (gClient^.gcPers.cpSelectedItem) <= 0 = return 0
        | otherwise = do
            item <- readRef (Ref (gClient^.gcPers.cpSelectedItem))
            imageIndex (item^.giIcon)
    setLayouts gClientRef = do
        gClient <- readRef gClientRef
        deathmatch <- fmap (^.cvValue) deathmatchCVar
        layout <- getLayout gClient deathmatch
        modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statLayouts .~ layout)
    getLayout :: GClientT -> Float -> Quake Int16
    getLayout gClient deathmatch
        | deathmatch /= 0 = do
            intermissionTime <- use (gameBaseGlobals.gbLevel.llIntermissionTime)
            let v = if (gClient^.gcPers.cpHealth) <= 0 || intermissionTime /= 0 || (gClient^.gcShowScores) then  1 else 0
                v' = if (gClient^.gcShowInventory) && (gClient^.gcPers.cpHealth) > 0 then v .|. 2 else v
            return v'
        | otherwise = do
            let v = if (gClient^.gcShowScores) || (gClient^.gcShowHelp) then 1 else 0
                v' = if (gClient^.gcShowInventory) && (gClient^.gcPers.cpHealth) > 0 then v .|. 2 else v
            return v'
    setFrags gClientRef = do
        gClient <- readRef gClientRef
        modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statFrags .~ fromIntegral (gClient^.gcResp.crScore))
    setHelpIcon gClientRef = do
        gClient <- readRef gClientRef
        frameNum <- use (gameBaseGlobals.gbLevel.llFrameNum)
        imageIndex <- use (gameBaseGlobals.gbGameImport.giImageIndex)
        icon <- getHelpIcon gClient frameNum imageIndex
        modifyRef gClientRef (\v -> v & gcPlayerState.psStats.ix Constants.statHelpIcon .~ fromIntegral icon)
    getHelpIcon gClient frameNum imageIndex
        | (gClient^.gcPers.cpHelpChanged) /= 0 && frameNum .&. 8 /= 0 =
            imageIndex (Just "i_help")
        | ((gClient^.gcPers.cpHand) == Constants.centerHanded || (gClient^.gcPlayerState.psFOV) > 91) && isJust (gClient^.gcPers.cpWeapon) = do
            weaponRef <- getWeaponRef (gClient^.gcPers.cpWeapon)
            weapon <- readRef weaponRef
            imageIndex (weapon^.giIcon)
        | otherwise = return 0
    getWeaponRef Nothing = do
        Com.fatalError "PlayerHud.setStats#doSetHelpicon gClient^.gcPers.cpWeapon is Nothing"
        return (Ref (-1))
    getWeaponRef (Just weaponRef) = return weaponRef
