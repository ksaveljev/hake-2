{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.PlayerHud where

import Control.Lens (use, preuse, ix, (^.), (.=), zoom, (%=), (&), (%~))
import Control.Monad (when, liftM)
import Data.Bits ((.|.), (.&.), complement)
import Data.Maybe (isJust)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Game.CVarT
import Game.LevelLocalsT
import Game.GameLocalsT
import Game.GItemT
import {-# SOURCE #-} Game.GameImportT
import Game.PlayerStateT
import Game.EdictT
import Game.GClientT
import Game.ClientRespawnT
import Game.ClientPersistantT
import Types
import QuakeRef
import QuakeState
import CVarVariables
import qualified Constants
import qualified Game.GameItems as GameItems

moveClientToIntermission :: Ref EdictT -> Quake ()
moveClientToIntermission _ = do
    io (putStrLn "PlayerHud.moveClientToIntermission") >> undefined -- TODO

setStats :: Ref EdictT -> Quake ()
setStats edictRef = do
    edict <- readRef edictRef
    let Just gClientRef@(Ref gClientIdx) = edict^.eClient

    -- health
    setHealth gClientRef

    -- ammo
    setAmmo gClientRef

    -- armor
    setArmor gClientRef

    -- pickup message
    setPickupMessage gClientRef

    -- timers
    setTimers gClientRef

    -- selected item
    setSelectedItem gClientRef

    -- layouts
    setLayouts gClientRef

    -- frags
    setFrags gClientRef

    -- help icon / current weapon if not shown
    setHelpIcon gClientRef

    gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats.ix Constants.statSpectator .= 0

  -- TODO: instead of using zoom + ix maybe use %= with V.//
  -- TODO: a good candidate for optimizations
  where setHealth :: Ref GClientT -> Quake ()
        setHealth (Ref gClientIdx) = do
          picHealth <- use $ gameBaseGlobals.gbLevel.llPicHealth
          edict <- readRef edictRef

          zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats) $ do
            ix Constants.statHealthIcon .= fromIntegral picHealth
            ix Constants.statHealth .= fromIntegral (edict^.eHealth)

        setAmmo :: Ref GClientT -> Quake ()
        setAmmo (Ref gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          if (gClient^.gcAmmoIndex) == 0
            then do
              zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats) $ do
                ix Constants.statAmmoIcon .= 0
                ix Constants.statAmmo .= 0
            else do
              Just item <- preuse $ gameBaseGlobals.gbItemList.ix (gClient^.gcAmmoIndex)
              imageIndex <- use $ gameBaseGlobals.gbGameImport.giImageIndex
              idx <- imageIndex (item^.giIcon)

              zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats) $ do
                ix Constants.statAmmoIcon .= fromIntegral idx
                ix Constants.statAmmo .= fromIntegral ((gClient^.gcPers.cpInventory) UV.! (gClient^.gcAmmoIndex))

        setArmor :: Ref GClientT -> Quake ()
        setArmor (Ref gClientIdx) = do
          edict <- readRef edictRef
          powerArmorType <- GameItems.powerArmorType edictRef

          gameImport <- use $ gameBaseGlobals.gbGameImport
          let sound = gameImport^.giSound
              soundIndex = gameImport^.giSoundIndex
              imageIndex = gameImport^.giImageIndex

          (powerArmorType', cells) <-
            if powerArmorType /= 0
              then do
                Just (GItemReference itemIdx) <- GameItems.findItem "cells"
                Just item <- preuse $ gameBaseGlobals.gbItemList.ix itemIdx
                Just cells <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPers.cpInventory.ix (item^.giIndex)

                if cells == 0 -- ran out of cells for power armor
                  then do
                    modifyRef edictRef (\v -> v & eFlags %~ (.&. (complement Constants.flPowerArmor)))
                    idx <- soundIndex (Just "misc/power2.wav")
                    sound (Just edictRef) Constants.chanItem idx 1 Constants.attnNorm 0
                    return (0, cells)
                  else
                    return (powerArmorType, cells)
              else
                return (0, 0)

          index <- GameItems.armorIndex edictRef
          frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          if | powerArmorType' /= 0 && (index == 0 || frameNum .&. 8 /= 0) -> do -- flash between power armor and other armor icon
                 idx <- imageIndex (Just "i_powershield")
                 zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats) $ do
                   ix Constants.statArmorIcon .= fromIntegral idx
                   ix Constants.statArmor .= fromIntegral cells

             | index /= 0 -> do
                 Just item <- GameItems.getItemByIndex index
                 idx <- imageIndex (item^.giIcon)
                 zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats) $ do
                   ix Constants.statArmorIcon .= fromIntegral idx
                   ix Constants.statArmor .= fromIntegral ((gClient^.gcPers.cpInventory) UV.! index)

             | otherwise -> do
                 zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats) $ do
                   ix Constants.statArmorIcon .= fromIntegral 0
                   ix Constants.statArmor .= fromIntegral 0

        setPickupMessage :: Ref GClientT -> Quake ()
        setPickupMessage (Ref gClientIdx) = do
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          when (levelTime > (gClient^.gcPickupMsgTime)) $ do
            zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats) $ do
              ix Constants.statPickupIcon .= 0
              ix Constants.statPickupString .= 0

        setTimers :: Ref GClientT -> Quake ()
        setTimers (Ref gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          imageIndex <- use $ gameBaseGlobals.gbGameImport.giImageIndex
          frameNum <- liftM fromIntegral $ use (gameBaseGlobals.gbLevel.llFrameNum)

          (icon, timer) <- if | (gClient^.gcQuadFrameNum) > frameNum -> do
                                  idx <- imageIndex (Just "p_quad")
                                  return (idx, truncate (((gClient^.gcQuadFrameNum) - frameNum) / 10))

                              | (gClient^.gcInvincibleFrameNum) > frameNum -> do
                                  idx <- imageIndex (Just "p_invulnerability")
                                  return (idx, truncate (((gClient^.gcInvincibleFrameNum) - frameNum) / 10))

                              | (gClient^.gcEnviroFrameNum) > frameNum -> do
                                  idx <- imageIndex (Just "p_envirosuit")
                                  return (idx, truncate (((gClient^.gcEnviroFrameNum) - frameNum) / 10))

                              | (gClient^.gcBreatherFrameNum) > frameNum -> do
                                  idx <- imageIndex (Just "p_rebreather")
                                  return (idx, truncate (((gClient^.gcBreatherFrameNum) - frameNum) / 10))

                              | otherwise -> return (0, 0)

          zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats) $ do
            ix Constants.statTimerIcon .= fromIntegral icon
            ix Constants.statTimer .= timer

        setSelectedItem :: Ref GClientT -> Quake ()
        setSelectedItem (Ref gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          imageIndex <- use $ gameBaseGlobals.gbGameImport.giImageIndex

          icon <- if (gClient^.gcPers.cpSelectedItem) <= 0
                    then
                      return 0
                    else do
                      Just item <- preuse $ gameBaseGlobals.gbItemList.ix (gClient^.gcPers.cpSelectedItem)
                      imageIndex (item^.giIcon)

          zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats) $ do
            ix Constants.statSelectedIcon .= fromIntegral icon
            ix Constants.statSelectedItem .= fromIntegral (gClient^.gcPers.cpSelectedItem)

        setLayouts :: Ref GClientT -> Quake ()
        setLayouts (Ref gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          deathmatchValue <- liftM (^.cvValue) deathmatchCVar
          let v = 0

          v' <- if deathmatchValue /= 0
                  then do
                    intermissionTime <- use $ gameBaseGlobals.gbLevel.llIntermissionTime
                    let v' = if (gClient^.gcPers.cpHealth) <= 0 || intermissionTime /= 0 || (gClient^.gcShowScores)
                               then v .|. 1
                               else v
                        v'' = if (gClient^.gcShowInventory) && (gClient^.gcPers.cpHealth) > 0
                                then v' .|. 2
                                else v'
                    return v''
                  else do
                    let v' = if (gClient^.gcShowScores) || (gClient^.gcShowHelp)
                               then v .|. 1
                               else v
                        v'' = if (gClient^.gcShowInventory) && (gClient^.gcPers.cpHealth) > 0
                                then v' .|. 2
                                else v'
                    return v''

          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats.ix Constants.statLayouts .= v'

        setFrags :: Ref GClientT -> Quake ()
        setFrags (Ref gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats.ix Constants.statFrags .= fromIntegral (gClient^.gcResp.crScore)

        setHelpIcon :: Ref GClientT -> Quake ()
        setHelpIcon (Ref gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          frameNum <- use $ gameBaseGlobals.gbLevel.llFrameNum
          imageIndex <- use $ gameBaseGlobals.gbGameImport.giImageIndex

          if | (gClient^.gcPers.cpHelpChanged) /= 0 && frameNum .&. 8 /= 0 -> do
                 idx <- imageIndex (Just "i_help")
                 gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats.ix Constants.statHelpIcon .= fromIntegral idx

             | ((gClient^.gcPers.cpHand) == Constants.centerHanded || (gClient^.gcPlayerState.psFOV) > 91) && isJust (gClient^.gcPers.cpWeapon) -> do
                 let Just (GItemReference weaponIdx) = gClient^.gcPers.cpWeapon
                 Just weapon <- preuse $ gameBaseGlobals.gbItemList.ix weaponIdx
                 idx <- imageIndex (weapon^.giIcon)
                 gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats.ix Constants.statHelpIcon .= fromIntegral idx

             | otherwise ->
                 gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats.ix Constants.statHelpIcon .= 0

setSpectatorStats :: Ref EdictT -> Quake ()
setSpectatorStats _ = do
    io (putStrLn "PlayerHud.setSpectatorStats") >> undefined -- TODO

checkChaseStats :: Ref EdictT -> Quake ()
checkChaseStats edictRef = do
    clients <- use $ gameBaseGlobals.gbGame.glClients
    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar

    setChaseStats clients 1 maxClientsValue

  where setChaseStats :: V.Vector GClientT -> Int -> Int -> Quake ()
        setChaseStats clients idx maxIdx
          | idx > maxIdx = return ()
          | otherwise = do
              let newEdictRef = Ref idx
              newEdict <- readRef newEdictRef

              let Just (Ref gClientIdx) = newEdict^.eClient

              if not(newEdict^.eInUse) || ((clients V.! gClientIdx)^.gcChaseTarget) /= Just edictRef
                then
                  setChaseStats clients (idx + 1) maxIdx

                else do
                  edict <- readRef edictRef
                  let Just (Ref edictClientIdx) = edict^.eClient

                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats .= ((clients V.! edictClientIdx)^.gcPlayerState.psStats)

                  setSpectatorStats newEdictRef
                  setChaseStats clients (idx + 1) maxIdx

deathmatchScoreboardMessage :: Ref EdictT -> Maybe (Ref EdictT) -> Quake ()
deathmatchScoreboardMessage _ _ = do
    io (putStrLn "PlayerHud.deathmatchScoreboardMessage") >> undefined -- TODO
