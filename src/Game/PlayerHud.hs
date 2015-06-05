{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.PlayerHud where

import Control.Lens (use, preuse, ix, (^.), (.=), zoom, (%=))
import Control.Monad (when, liftM)
import Data.Bits ((.|.), (.&.), complement)
import Data.Maybe (isJust)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Game.GameItems as GameItems

moveClientToIntermission :: EdictReference -> Quake ()
moveClientToIntermission _ = do
    io (putStrLn "PlayerHud.moveClientToIntermission") >> undefined -- TODO

setStats :: EdictReference -> Quake ()
setStats edictRef@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    let Just gClientRef@(GClientReference gClientIdx) = edict^.eClient

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
  where setHealth :: GClientReference -> Quake ()
        setHealth (GClientReference gClientIdx) = do
          picHealth <- use $ gameBaseGlobals.gbLevel.llPicHealth
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats) $ do
            ix Constants.statHealthIcon .= fromIntegral picHealth
            ix Constants.statHealth .= fromIntegral (edict^.eEdictStatus.eHealth)

        setAmmo :: GClientReference -> Quake ()
        setAmmo (GClientReference gClientIdx) = do
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

        setArmor :: GClientReference -> Quake ()
        setArmor (GClientReference gClientIdx) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
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
                    gameBaseGlobals.gbGEdicts.ix edictIdx.eFlags %= (.&. (complement Constants.flPowerArmor))
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

        setPickupMessage :: GClientReference -> Quake ()
        setPickupMessage (GClientReference gClientIdx) = do
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx

          when (levelTime > (gClient^.gcPickupMsgTime)) $ do
            zoom (gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats) $ do
              ix Constants.statPickupIcon .= 0
              ix Constants.statPickupString .= 0

        setTimers :: GClientReference -> Quake ()
        setTimers (GClientReference gClientIdx) = do
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

        setSelectedItem :: GClientReference -> Quake ()
        setSelectedItem (GClientReference gClientIdx) = do
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

        setLayouts :: GClientReference -> Quake ()
        setLayouts (GClientReference gClientIdx) = do
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

        setFrags :: GClientReference -> Quake ()
        setFrags (GClientReference gClientIdx) = do
          Just gClient <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx
          gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats.ix Constants.statFrags .= fromIntegral (gClient^.gcResp.crScore)

        setHelpIcon :: GClientReference -> Quake ()
        setHelpIcon (GClientReference gClientIdx) = do
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

setSpectatorStats :: EdictReference -> Quake ()
setSpectatorStats _ = do
    io (putStrLn "PlayerHud.setSpectatorStats") >> undefined -- TODO

checkChaseStats :: EdictReference -> Quake ()
checkChaseStats edictRef@(EdictReference edictIdx) = do
    edicts <- use $ gameBaseGlobals.gbGEdicts
    clients <- use $ gameBaseGlobals.gbGame.glClients
    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar

    setChaseStats edicts clients 1 maxClientsValue

  where setChaseStats :: V.Vector EdictT -> V.Vector GClientT -> Int -> Int -> Quake ()
        setChaseStats edicts clients idx maxIdx
          | idx > maxIdx = return ()
          | otherwise = do
              let Just (GClientReference gClientIdx) = (edicts V.! idx)^.eClient

              if not((edicts V.! idx)^.eInUse) || ((clients V.! gClientIdx)^.gcChaseTarget) /= Just edictRef
                then setChaseStats edicts clients (idx + 1) maxIdx
                else do
                  let Just (GClientReference edictClientIdx) = (edicts V.! edictIdx)^.eClient

                  gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats .= ((clients V.! edictClientIdx)^.gcPlayerState.psStats)

                  setSpectatorStats (EdictReference idx)
                  setChaseStats edicts clients (idx + 1) maxIdx
