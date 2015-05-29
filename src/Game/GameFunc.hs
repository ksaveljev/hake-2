{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameFunc where

import Control.Lens (use, preuse, (.=), (^.), ix, zoom, (%=), (-=))
import Control.Monad (when, liftM, void, unless)
import Data.Bits ((.&.), (.|.), complement)
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, isNothing)
import Linear (V3(..), _x, _y, _z, normalize, quadrance)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib

trainStartOn :: Int
trainStartOn = 1

trainToggle :: Int
trainToggle = 2

trainBlockStops :: Int
trainBlockStops = 4

{-
- PLATS
- 
- movement options:
- 
- linear smooth start, hard stop smooth start, smooth stop
- 
- start end acceleration speed deceleration begin sound end sound target
- fired when reaching end wait at end
- 
- object characteristics that use move segments
- --------------------------------------------- movetype_push, or
- movetype_stop action when touched action when blocked action when used
- disabled? auto trigger spawning
- 
-}

spFuncButton :: EntThink
spFuncButton =
  GenericEntThink "sp_func_button" $ \er@(EdictReference edictIdx) -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let soundIndex = gameImport^.giSoundIndex
        setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    GameBase.setMoveDir (gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esAngles) (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eMoveDir)

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypeStop
      eSolid .= Constants.solidBsp

    setModel er (edict^.eEdictInfo.eiModel)

    when ((edict^.eSounds) /= 1) $ do
      soundIndex (Just "switches/butn2.wav") >>= (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miSoundStart .=)

    when ((edict^.eEdictPhysics.eSpeed) == 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eSpeed .= 40

    Just speed <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eSpeed

    when ((edict^.eEdictPhysics.eAccel) == 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eAccel .= speed

    when ((edict^.eEdictPhysics.eDecel) == 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eDecel .= speed

    when ((edict^.eWait) == 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eWait .= 3
    
    lip <- use $ gameBaseGlobals.gbSpawnTemp.stLip
    when (lip == 0) $
      gameBaseGlobals.gbSpawnTemp.stLip .= 4

    let origin = edict^.eEntityState.esOrigin
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.ePos1 .= origin

    lip' <- use $ gameBaseGlobals.gbSpawnTemp.stLip
    let moveDir = edict^.eEdictPhysics.eMoveDir
        absMoveDir = fmap abs moveDir
        size = edict^.eEdictMinMax.eSize
        dist = (absMoveDir^._x) * (size^._x)
             + (absMoveDir^._y) * (size^._y)
             + (absMoveDir^._z) * (size^._z)
             - (fromIntegral lip')

    let pos2 = origin + fmap (* dist) moveDir
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.ePos2 .= pos2

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eEdictAction.eaUse .= Just buttonUse
      eEntityState.esEffects %= (.|. Constants.efAnim01)

    if (edict^.eEdictStatus.eHealth) /= 0
      then
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEdictStatus.eMaxHealth .= (edict^.eEdictStatus.eHealth)
          eEdictAction.eaDie .= Just buttonKilled
          eEdictStatus.eTakeDamage .= Constants.damageYes
      else
        when (isNothing (edict^.eEdictInfo.eiTargetName)) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaTouch .= Just buttonTouch

    Just updatedEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo) $ do
      miState .= Constants.stateBottom
      miSpeed .= (updatedEdict^.eEdictPhysics.eSpeed)
      miAccel .= (updatedEdict^.eEdictPhysics.eAccel)
      miDecel .= (updatedEdict^.eEdictPhysics.eDecel)
      miWait .= (updatedEdict^.eWait)
      miStartOrigin .= (updatedEdict^.eEdictPhysics.ePos1)
      miStartAngles .= (updatedEdict^.eEntityState.esAngles)
      miEndOrigin .= (updatedEdict^.eEdictPhysics.ePos2)
      miEndAngles .= (updatedEdict^.eEntityState.esAngles)

    linkEntity er
    return True

spFuncDoor :: EntThink
spFuncDoor =
  GenericEntThink "sp_func_door" $ \er@(EdictReference edictIdx) -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let soundIndex = gameImport^.giSoundIndex
        setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    GameBase.setMoveDir (gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esAngles) (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eMoveDir)

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    when ((edict^.eSounds) /= 1) $ do
      soundIndex (Just "doors/dr1_strt.wav") >>= (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miSoundStart .=)
      soundIndex (Just "doors/dr1_mid.wav") >>= (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miSoundMiddle .=)
      soundIndex (Just "doors/dr1_end.wav") >>= (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miSoundEnd .=)

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypePush
      eSolid .= Constants.solidBsp

    setModel er (edict^.eEdictInfo.eiModel)

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction) $ do
      eaBlocked .= Just doorBlocked
      eaUse .= Just doorUse

    when ((edict^.eEdictPhysics.eSpeed) == 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eSpeed .= 100

    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    when (deathmatchValue /= 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eSpeed %= (* 2)

    Just speed <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eSpeed

    when ((edict^.eEdictPhysics.eAccel) == 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eAccel .= speed

    when ((edict^.eEdictPhysics.eDecel) == 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eDecel .= speed

    when ((edict^.eWait) == 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eWait .= 3

    when ((edict^.eEdictStatus.eDmg) == 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictStatus.eDmg .= 2
    
    lip <- use $ gameBaseGlobals.gbSpawnTemp.stLip
    when (lip == 0) $
      gameBaseGlobals.gbSpawnTemp.stLip .= 8

    -- calculate second position
    let origin = edict^.eEntityState.esOrigin
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.ePos1 .= origin

    lip' <- use $ gameBaseGlobals.gbSpawnTemp.stLip
    let moveDir = edict^.eEdictPhysics.eMoveDir
        absMoveDir = fmap abs moveDir
        size = edict^.eEdictMinMax.eSize
        dist = (absMoveDir^._x) * (size^._x)
             + (absMoveDir^._y) * (size^._y)
             + (absMoveDir^._z) * (size^._z)
             - (fromIntegral lip')

    gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miDistance .= dist

    let pos2 = origin + fmap (* dist) moveDir
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.ePos2 .= pos2

    -- if it starts open, switch the positions
    when ((edict^.eSpawnFlags) .&. Constants.doorStartOpen /= 0) $ do
      zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
        eEntityState.esOrigin .= pos2
        eEdictPhysics.ePos2 .= origin
        eEdictPhysics.ePos1 .= pos2

    gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miState .= Constants.stateBottom

    if (edict^.eEdictStatus.eHealth) /= 0
      then
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEdictStatus.eTakeDamage .= Constants.damageYes
          eEdictStatus.eMaxHealth .= (edict^.eEdictStatus.eHealth)
          eEdictAction.eaDie .= Just doorKilled
      else
        when (isJust (edict^.eEdictInfo.eiTargetName) && isJust (edict^.eEdictInfo.eiMessage)) $ do
          void $ soundIndex (Just "misc/talk.wav")
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaTouch .= Just doorTouch

    Just updatedEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo) $ do
      miSpeed .= (updatedEdict^.eEdictPhysics.eSpeed)
      miAccel .= (updatedEdict^.eEdictPhysics.eAccel)
      miDecel .= (updatedEdict^.eEdictPhysics.eDecel)
      miWait .= (updatedEdict^.eWait)
      miStartOrigin .= (updatedEdict^.eEdictPhysics.ePos1)
      miStartAngles .= (updatedEdict^.eEntityState.esAngles)
      miEndOrigin .= (updatedEdict^.eEdictPhysics.ePos2)
      miEndAngles .= (updatedEdict^.eEntityState.esAngles)

    when ((updatedEdict^.eSpawnFlags) .&. 16 /= 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEffects %= (.|. Constants.efAnimAll)

    when ((updatedEdict^.eSpawnFlags) .&. 64 /= 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEffects %= (.|. Constants.efAnimAllFast)

    -- to simplify logic elsewhere, make non-teamed doors into a team of one
    when (isNothing (updatedEdict^.eEdictInfo.eiTeam)) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictOther.eoTeamMaster .= Just er

    linkEntity er

    time <- use $ gameBaseGlobals.gbLevel.llTime
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaNextThink .= time + Constants.frameTime

    let nextThink = if (updatedEdict^.eEdictStatus.eHealth) /= 0 || isJust (updatedEdict^.eEdictInfo.eiTargetName)
                      then thinkCalcMoveSpeed
                      else thinkSpawnDoorTrigger

    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaThink .= Just nextThink

    return True

spFuncDoorSecret :: EntThink
spFuncDoorSecret =
  GenericEntThink "sp_func_door_secret" $ \_ -> do
    io (putStrLn "GameFunc.spFuncDoorSecret") >> undefined -- TODO

spFuncDoorRotating :: EntThink
spFuncDoorRotating =
  GenericEntThink "sp_func_door_rotating" $ \_ -> do
    io (putStrLn "GameFunc.spFuncDoorRotating") >> undefined -- TODO

spFuncConveyor :: EntThink
spFuncConveyor =
  GenericEntThink "sp_func_conveyor" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    when ((self^.eEdictPhysics.eSpeed) == 0) $
      gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictPhysics.eSpeed .= 100

    when ((self^.eSpawnFlags) .&. 1 == 0) $ do
      Just speed <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictPhysics.eSpeed
      gameBaseGlobals.gbGEdicts.ix selfIdx.eCount .= truncate speed
      gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictPhysics.eSpeed .= 0

    gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictAction.eaUse .= Just funcConveyorUse
    
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    setModel selfRef (self^.eEdictInfo.eiModel)
    gameBaseGlobals.gbGEdicts.ix selfIdx.eSolid .= Constants.solidBsp
    linkEntity selfRef
    return True

{-
- QUAKED func_killbox (1 0 0) ? Kills everything inside when fired,
- irrespective of protection.
-}
useKillBox :: EntUse
useKillBox =
  GenericEntUse "use_killbox" $ \selfRef _ _ ->
    void $ GameUtil.killBox selfRef

spFuncKillBox :: EntThink
spFuncKillBox =
  GenericEntThink "sp_func_killbox" $ \edictRef@(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    setModel <- use $ gameBaseGlobals.gbGameImport.giSetModel

    setModel edictRef (edict^.eEdictInfo.eiModel)

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eEdictAction.eaUse .= Just useKillBox
      eSvFlags .= Constants.svfNoClient

    return True

spFuncRotating :: EntThink
spFuncRotating =
  GenericEntThink "sp_func_rotating" $ \edictRef@(EdictReference edictIdx) -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    gameBaseGlobals.gbGEdicts.ix edictIdx.eSolid .= Constants.solidBsp

    updateMoveType edictRef

    -- set the axis of rotation
    updateRotationAxis edictRef

    -- check for reverse rotation
    checkRevereseRotation edictRef

    checkSpeedAndDmg edictRef

    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaUse .= Just rotatingUse

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    when ((edict^.eEdictStatus.eDmg) /= 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaBlocked .= Just rotatingBlocked

    when ((edict^.eSpawnFlags) .&. 1 /= 0) $
      entUse (fromJust $ edict^.eEdictAction.eaUse) edictRef Nothing Nothing

    when ((edict^.eSpawnFlags) .&. 64 /= 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEffects %= (.|. Constants.efAnimAll)

    when ((edict^.eSpawnFlags) .&. 128 /= 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEffects %= (.|. Constants.efAnimAllFast)

    setModel edictRef (edict^.eEdictInfo.eiModel)
    linkEntity edictRef

    return True

  where updateMoveType :: EdictReference -> Quake ()
        updateMoveType (EdictReference edictIdx) = do
          Just spawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags

          let moveType = if spawnFlags .&. 32 /= 0
                           then Constants.moveTypeStop
                           else Constants.moveTypePush

          gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveType .= moveType

        updateRotationAxis :: EdictReference -> Quake ()
        updateRotationAxis (EdictReference edictIdx) = do
          Just spawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags

          let moveDir = if | spawnFlags .&. 4 /= 0 -> V3 0 0 1
                           | spawnFlags .&. 8 /= 0 -> V3 1 0 0
                           | otherwise -> V3 0 1 0

          gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eMoveDir .= moveDir

        checkRevereseRotation :: EdictReference -> Quake ()
        checkRevereseRotation (EdictReference edictIdx) = do
          Just spawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags

          when (spawnFlags .&. 2 /= 0) $
            gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eMoveDir %= (fmap (0 -))

        checkSpeedAndDmg :: EdictReference -> Quake ()
        checkSpeedAndDmg (EdictReference edictIdx) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          when ((edict^.eEdictPhysics.eSpeed) == 0) $
            gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eSpeed .= 100

          when ((edict^.eEdictStatus.eDmg) == 0) $
            gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictStatus.eDmg .= 2


spTriggerElevator :: EntThink
spTriggerElevator =
  GenericEntThink "sp_trigger_elevator" $ \_ -> do
    io (putStrLn "GameFunc.spTriggerElevator") >> undefined -- TODO

spFuncPlat :: EdictReference -> Quake ()
spFuncPlat _ = io (putStrLn "GameFunc.spFuncPlat") >> undefined -- TODO

spFuncWater :: EdictReference -> Quake ()
spFuncWater _ = io (putStrLn "GameFunc.spFuncWater") >> undefined -- TODO

spFuncTrain :: EdictReference -> Quake ()
spFuncTrain er@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let setModel = gameImport^.giSetModel
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity
        dprintf = gameImport^.giDprintf

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypePush
      eEntityState.esAngles .= V3 0 0 0
      eEdictAction.eaBlocked .= Just trainBlocked
      eSolid .= Constants.solidBsp

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    if (edict^.eSpawnFlags) .&. trainBlockStops /= 0
      then gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictStatus.eDmg .= 0
      else
        when ((edict^.eEdictStatus.eDmg) == 0) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictStatus.eDmg .= 100

    setModel er (edict^.eEdictInfo.eiModel)

    noise <- use $ gameBaseGlobals.gbSpawnTemp.stNoise
    when (isJust noise) $ do
      noiseIdx <- soundIndex noise
      gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miSoundMiddle .= noiseIdx

    when ((edict^.eEdictPhysics.eSpeed) == 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eSpeed .= 100

    Just selfSpeed <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eSpeed
    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveInfo.miSpeed .= selfSpeed
      eMoveInfo.miAccel .= selfSpeed
      eMoveInfo.miDecel .= selfSpeed
      eEdictAction.eaUse .= Just trainUse

    linkEntity er

    if isJust (edict^.eEdictInfo.eiTarget)
      then do
        time <- use $ gameBaseGlobals.gbLevel.llTime
        -- start trains on the second frame, to make sure their targets
        -- have had a chance to spawn
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction) $ do
          eaThink .= Just funcTrainFind
          eaNextThink .= time + Constants.frameTime
      else
        dprintf $ "func_train without a target at " `B.append` Lib.vtos (edict^.eEdictMinMax.eAbsMin) `B.append` "\n"

spFuncTimer :: EdictReference -> Quake ()
spFuncTimer er@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    when ((edict^.eWait) == 0) $
      gameBaseGlobals.gbGEdicts.ix edictIdx.eWait .= 1

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eEdictAction.eaUse .= Just funcTimerUse
      eEdictAction.eaThink .= Just funcTimerThink

    when ((edict^.eRandom) >= (edict^.eWait)) $ do
      gameBaseGlobals.gbGEdicts.ix edictIdx.eRandom .= (edict^.eWait) - Constants.frameTime
      dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
      dprintf $ "func_timer at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` " has random >= wait\n"
    
    when (((edict^.eSpawnFlags) .&. 1) /= 0) $ do
      time <- use $ gameBaseGlobals.gbLevel.llTime
      pauseTime <- use $ gameBaseGlobals.gbSpawnTemp.stPauseTime
      cr <- Lib.crandom
      zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
        eEdictAction.eaNextThink .= time + 1 + pauseTime + (edict^.eDelay) + (edict^.eWait) + cr * (edict^.eRandom)
        eEdictOther.eoActivator .= Just er

    gameBaseGlobals.gbGEdicts.ix edictIdx.eSvFlags .= Constants.svfNoClient

funcTimerUse :: EntUse
funcTimerUse =
  GenericEntUse "func_timer_use" $ \_ _ _ -> do
    io (putStrLn "GameFunc.funcTimerUse") >> undefined -- TODO

{-
- QUAKED func_timer (0.3 0.1 0.6) (-8 -8 -8) (8 8 8) START_ON "wait" base
- time between triggering all targets, default is 1 "random" wait variance,
- default is 0
- 
- so, the basic time between firing is a random time between (wait -
- random) and (wait + random)
- 
- "delay" delay before first firing when turned on, default is 0
- 
- "pausetime" additional delay used only the very first time and only if
- spawned with START_ON
- 
- These can used but not touched.
-}
funcTimerThink :: EntThink
funcTimerThink =
  GenericEntThink "func_timer_think" $ \edictRef@(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    GameUtil.useTargets edictRef (edict^.eEdictOther.eoActivator)

    time <- use $ gameBaseGlobals.gbLevel.llTime
    r <- Lib.crandom
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaNextThink .= time + (edict^.eWait) + r * (edict^.eRandom)

    return True

trainBlocked :: EntBlocked
trainBlocked =
  GenericEntBlocked "train_blocked" $ \_ _ -> do
    io (putStrLn "GameFunc.trainBlocked") >> undefined -- TODO

trainUse :: EntUse
trainUse =
  GenericEntUse "train_use" $ \selfRef@(EdictReference selfIdx) _ activatorRef -> do
    gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictOther.eoActivator .= activatorRef

    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self^.eSpawnFlags) .&. trainStartOn /= 0
      then
        unless ((self^.eSpawnFlags) .&. trainToggle == 0) $ do
          zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
            eSpawnFlags %= (.&. (complement trainStartOn))
            eEdictPhysics.eVelocity .= V3 0 0 0
            eEdictAction.eaNextThink .= 0
      else
        if isJust (self^.eTargetEnt)
          then trainResume selfRef
          else void $ think trainNext selfRef

funcTrainFind :: EntThink
funcTrainFind =
  GenericEntThink "func_train_find" $ \er@(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let dprintf = gameImport^.giDprintf
        linkEntity = gameImport^.giLinkEntity

    if isNothing (edict^.eEdictInfo.eiTarget)
      then
        dprintf "train_find: no target\n"
      else do
        entRef <- GameBase.pickTarget (edict^.eEdictInfo.eiTarget)

        if isNothing entRef
          then
            dprintf $ "train_find: target " `B.append` (fromJust $ edict^.eEdictInfo.eiTarget) `B.append` " not found\n"
          else do
            let Just (EdictReference entIdx) = entRef
            Just ent <- preuse $ gameBaseGlobals.gbGEdicts.ix entIdx

            let origin = edict^.eEntityState.esOrigin
                mins = edict^.eEdictMinMax.eMins

            zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
              eEdictInfo.eiTarget .= (ent^.eEdictInfo.eiTarget)
              eEntityState.esOrigin .= (origin - mins)

            linkEntity er

            -- if not triggered, start immediately
            when (isNothing (edict^.eEdictInfo.eiTargetName)) $
              gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags %= (.|. trainStartOn)

            Just updatedEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

            when ((updatedEdict^.eSpawnFlags) .&. trainStartOn /= 0) $ do
              time <- use $ gameBaseGlobals.gbLevel.llTime

              zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
                eEdictAction.eaNextThink .= time + Constants.frameTime
                eEdictAction.eaThink .= Just trainNext
                eEdictOther.eoActivator .= Just er

    return True

doorUse :: EntUse
doorUse =
  GenericEntUse "door_use" $ \selfRef@(EdictReference selfIdx) _ activatorRef -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    unless ((self^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
      done <- if (self^.eSpawnFlags) .&. Constants.doorToggle /= 0
                then if (self^.eMoveInfo.miState) == Constants.stateUp || (self^.eMoveInfo.miState) == Constants.stateTop
                       then do
                         -- trigger all paired doors
                         triggerPairedDoors activatorRef (Just selfRef)
                         return True
                       else return False
                else return False

      unless done $ do
        -- trigger all paired doors
        triggerPairedDoors activatorRef (Just selfRef)

  where triggerPairedDoors :: Maybe EdictReference -> Maybe EdictReference -> Quake ()
        triggerPairedDoors _ Nothing = return ()
        triggerPairedDoors activatorRef (Just edictRef@(EdictReference edictIdx)) = do
          Just teamChain <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictOther.eoTeamChain
          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
            eEdictInfo.eiMessage .= Nothing
            eEdictAction.eaTouch .= Nothing
          doorGoUp edictRef activatorRef
          triggerPairedDoors activatorRef teamChain

doorBlocked :: EntBlocked
doorBlocked =
  GenericEntBlocked "door_blocked" $ \_ _ -> do
    io (putStrLn "GameFunc.doorBlocked") >> undefined -- TODO

doorKilled :: EntDie
doorKilled =
  GenericEntDie "door_killed" $ \_ _ _ _ _ -> do
    io (putStrLn "GameFunc.doorKilled") >> undefined -- TODO

doorTouch :: EntTouch
doorTouch =
  GenericEntTouch "door_touch" $ \_ _ _ _ -> do
    io (putStrLn "GameFunc.doorTouch") >> undefined -- TODO

thinkCalcMoveSpeed :: EntThink
thinkCalcMoveSpeed =
  GenericEntThink "think_calc_movespeed" $ \er@(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    -- only the team master does this
    unless ((edict^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
      -- find the smallest distance any member of the team will be moving
      minDist <- findSmallestDistance (edict^.eEdictOther.eoTeamChain) (abs $ edict^.eMoveInfo.miDistance)

      let time = minDist / (edict^.eMoveInfo.miSpeed)

      -- adjust speeds so they will all complete at the same time
      adjustSpeeds (Just er) time

    return True

  where findSmallestDistance :: Maybe EdictReference -> Float -> Quake Float
        findSmallestDistance Nothing minDist = return minDist
        findSmallestDistance (Just (EdictReference entIdx)) minDist = do
          Just ent <- preuse $ gameBaseGlobals.gbGEdicts.ix entIdx
          let dist = abs (ent^.eMoveInfo.miDistance)
              minDist' = if dist < minDist then dist else minDist
          findSmallestDistance (ent^.eEdictOther.eoTeamChain) minDist'

        adjustSpeeds :: Maybe EdictReference -> Float -> Quake ()
        adjustSpeeds Nothing _ = return ()
        adjustSpeeds (Just (EdictReference entIdx)) time = do
          Just ent <- preuse $ gameBaseGlobals.gbGEdicts.ix entIdx

          let newspeed = (abs $ ent^.eMoveInfo.miSpeed) / time
              ratio = newspeed / (ent^.eMoveInfo.miSpeed)
              accel = if (ent^.eMoveInfo.miAccel) == (ent^.eMoveInfo.miSpeed)
                        then newspeed
                        else (ent^.eMoveInfo.miAccel) * ratio
              decel = if (ent^.eMoveInfo.miDecel) == (ent^.eMoveInfo.miSpeed)
                        then newspeed
                        else (ent^.eMoveInfo.miDecel) * ratio

          zoom (gameBaseGlobals.gbGEdicts.ix entIdx.eMoveInfo) $ do
            miAccel .= accel
            miDecel .= decel
            miSpeed .= newspeed

          adjustSpeeds (ent^.eEdictOther.eoTeamChain) time

thinkSpawnDoorTrigger :: EntThink
thinkSpawnDoorTrigger =
  GenericEntThink "think_spawn_door_trigger" $ \er@(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    -- only the team leader spawns a trigger
    unless ((edict^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
      let mins = edict^.eEdictMinMax.eMins
          maxs = edict^.eEdictMinMax.eMaxs

      (mins', maxs') <- teamChainAddPointToBound (edict^.eEdictOther.eoTeamChain) mins maxs

      let expandedMins = (V3 (-60) (-60) 0) + mins'
          expandedMaxs = (V3 60 60 0) + maxs'

      otherRef@(EdictReference otherIdx) <- GameUtil.spawn

      zoom (gameBaseGlobals.gbGEdicts.ix otherIdx) $ do
        eEdictMinMax.eMins .= expandedMins
        eEdictMinMax.eMaxs .= expandedMaxs
        eOwner .= Just er
        eSolid .= Constants.solidTrigger
        eMoveType .= Constants.moveTypeNone
        eEdictAction.eaTouch .= Just touchDoorTrigger

      linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
      linkEntity otherRef

      when ((edict^.eSpawnFlags) .&. Constants.doorStartOpen /= 0) $
        doorUseAreaPortals er True
        
      void $ think thinkCalcMoveSpeed er

    return True

  where teamChainAddPointToBound :: Maybe EdictReference -> V3 Float -> V3 Float -> Quake (V3 Float, V3 Float)
        teamChainAddPointToBound Nothing mins maxs = return (mins, maxs)
        teamChainAddPointToBound (Just (EdictReference otherIdx)) mins maxs = do
          Just other <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx
          let (mins', maxs') = GameBase.addPointToBound (other^.eEdictMinMax.eAbsMin) mins maxs
              (mins'', maxs'') = GameBase.addPointToBound (other^.eEdictMinMax.eAbsMax) mins' maxs'
          teamChainAddPointToBound (other^.eEdictOther.eoTeamChain) mins'' maxs''

buttonUse :: EntUse
buttonUse =
  GenericEntUse "button_use" $ \_ _ _ -> do
    io (putStrLn "GameFunc.buttonUse") >> undefined -- TODO

buttonTouch :: EntTouch
buttonTouch =
  GenericEntTouch "button_touch" $ \_ _ _ _ -> do
    io (putStrLn "GameFunc.buttonTouch") >> undefined -- TODO

buttonKilled :: EntDie
buttonKilled =
  GenericEntDie "button_killed" $ \_ _ _ _ _ -> do
    io (putStrLn "GameFunc.buttonKilled") >> undefined -- TODO

trainNext :: EntThink
trainNext =
  GenericEntThink "train_next" $ \er@(EdictReference edictIdx) -> do
    (done, entRef) <- pickNextTarget er True

    unless done $ do
      let Just (EdictReference entIdx) = entRef
      Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
      Just ent <- preuse $ gameBaseGlobals.gbGEdicts.ix entIdx

      zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
        eMoveInfo.miWait .= (ent^.eMoveInfo.miWait)
        eTargetEnt .= entRef

      when ((edict^.eFlags) .&. Constants.flTeamSlave == 0) $ do
        when ((edict^.eMoveInfo.miSoundStart) /= 0) $ do
          sound <- use $ gameBaseGlobals.gbGameImport.giSound
          sound (Just er) (Constants.chanNoPhsAdd + Constants.chanVoice) (edict^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0
        gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esSound .= (edict^.eMoveInfo.miSoundMiddle)

      let dest = (ent^.eEntityState.esOrigin) - (edict^.eEdictMinMax.eMins)

      zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo) $ do
        miState .= Constants.stateTop
        miStartOrigin .= (edict^.eEntityState.esOrigin)
        miEndOrigin .= dest

      moveCalc er dest trainWait

      gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags %= (.|. trainStartOn)

    return True

  where pickNextTarget :: EdictReference -> Bool -> Quake (Bool, Maybe EdictReference)
        pickNextTarget er@(EdictReference edictIdx) first = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          gameImport <- use $ gameBaseGlobals.gbGameImport
          let dprintf = gameImport^.giDprintf
              linkEntity = gameImport^.giLinkEntity

          if isNothing (self^.eEdictInfo.eiTarget)
            then return (True, Nothing)
            else do
              entRef <- GameBase.pickTarget (self^.eEdictInfo.eiTarget)

              if isNothing entRef
                then do
                  dprintf $ "train_next: bad target " `B.append` (fromJust $ self^.eEdictInfo.eiTarget) `B.append` "\n"
                  return (True, Nothing)
                else do
                  let Just (EdictReference entIdx) = entRef
                  Just ent <- preuse $ gameBaseGlobals.gbGEdicts.ix entIdx
                  gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictInfo.eiTarget .= (ent^.eEdictInfo.eiTarget)

                  -- check for a teleport path_corner
                  if (ent^.eSpawnFlags) .&. 1 /= 0
                    then
                      if not first
                        then do
                          dprintf $ "connected teleport path_corner, see " `B.append` (ent^.eClassName) `B.append` " at " `B.append` (Lib.vtos (ent^.eEntityState.esOrigin)) `B.append` "\n"
                          return (True, entRef)
                        else do
                          let origin = ((ent^.eEntityState.esOrigin) - (self^.eEdictMinMax.eMins))
                          zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState) $ do
                            esOrigin .= origin
                            esOldOrigin .= origin
                            esEvent .= Constants.evOtherTeleport

                          linkEntity er

                          pickNextTarget er False

                    else return (False, entRef)

touchDoorTrigger :: EntTouch
touchDoorTrigger =
  GenericEntTouch "touch_door_trigger" $ \(EdictReference selfIdx) otherRef@(EdictReference otherIdx) _ _ -> do
    Just other <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    let Just ownerRef@(EdictReference ownerIdx) = self^.eOwner
    Just owner <- preuse $ gameBaseGlobals.gbGEdicts.ix ownerIdx
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let skip = (other^.eEdictStatus.eHealth) <= 0 
            || ((other^.eSvFlags) .&. Constants.svfMonster == 0 && isNothing (other^.eClient))
            || ((owner^.eSpawnFlags) .&. Constants.doorNoMonster /= 0 && (other^.eSvFlags) .&. Constants.svfMonster /= 0)
            || levelTime < (self^.eEdictTiming.etTouchDebounceTime)

    unless skip $ do
      gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictTiming.etTouchDebounceTime .= levelTime + 1
      entUse doorUse ownerRef (Just otherRef) (Just otherRef)

{-
- DOORS
- 
- spawn a trigger surrounding the entire team unless it is already targeted
- by another.
- 
-}

{-
- QUAKED func_door (0 .5 .8) ? START_OPEN x CRUSHER NOMONSTER ANIMATED
- TOGGLE ANIMATED_FAST TOGGLE wait in both the start and end states for a
- trigger event. START_OPEN the door to moves to its destination when
- spawned, and operate in reverse. It is used to temporarily or permanently
- close off an area when triggered (not useful for touch or takedamage
- doors). NOMONSTER monsters will not trigger this door
- 
- "message" is printed when the door is touched if it is a trigger door and
- it hasn't been fired yet "angle" determines the opening direction
- "targetname" if set, no touch field will be spawned and a remote button
- or trigger field activates the door. "health" if set, door must be shot
- open "speed" movement speed (100 default) "wait" wait before returning (3
- default, -1 = never return) "lip" lip remaining at end of move (8
- default) "dmg" damage to inflict when blocked (2 default) "sounds" 1)
- silent 2) light 3) medium 4) heavy
-}

doorUseAreaPortals :: EdictReference -> Bool -> Quake ()
doorUseAreaPortals (EdictReference selfIdx) open = do
    Just maybeTarget <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictInfo.eiTarget

    case maybeTarget of
      Nothing -> return ()
      Just target -> setAreaPortals target Nothing

  where setAreaPortals :: B.ByteString -> Maybe EdictReference -> Quake ()
        setAreaPortals target ref = do
          maybeFoundRef <- GameBase.gFind ref GameBase.findByTarget target

          case maybeFoundRef of
            Nothing -> return ()
            Just (EdictReference foundIdx) -> do
              Just foundEdict <- preuse $ gameBaseGlobals.gbGEdicts.ix foundIdx
              when (BC.map toLower (foundEdict^.eClassName) == "func_areaportal") $ do
                setAreaPortalState <- use $ gameBaseGlobals.gbGameImport.giSetAreaPortalState
                setAreaPortalState (foundEdict^.eStyle) open
              setAreaPortals target maybeFoundRef

trainResume :: EdictReference -> Quake ()
trainResume _ = io (putStrLn "GameFunc.trainResume") >> undefined -- TODO

trainWait :: EntThink
trainWait =
  GenericEntThink "train_wait" $ \edictRef@(EdictReference edictIdx) -> do
    done <- checkPathTarget edictRef

    unless done $ do
      Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

      if edict^.eMoveInfo.miWait /= 0
        then do
          if | edict^.eMoveInfo.miWait > 0 -> do
                 time <- use $ gameBaseGlobals.gbLevel.llTime

                 zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction) $ do
                   eaNextThink .= time + (edict^.eMoveInfo.miWait)
                   eaThink .= Just trainNext

             | (edict^.eSpawnFlags) .&. trainToggle /= 0 -> do
                 void $ think trainNext edictRef

                 zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
                   eSpawnFlags %= (.&. (complement trainStartOn))
                   eEdictPhysics.eVelocity .= V3 0 0 0
                   eEdictAction.eaNextThink .= 0

             | otherwise -> return ()

          when ((edict^.eFlags) .&. Constants.flTeamSlave == 0) $ do
            when ((edict^.eMoveInfo.miSoundEnd) /= 0) $ do
              sound <- use $ gameBaseGlobals.gbGameImport.giSound
              sound (Just edictRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (edict^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0

            gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esSound .= 0

        else void $ think trainNext edictRef

    return True

  where checkPathTarget :: EdictReference -> Quake Bool
        checkPathTarget (EdictReference edictIdx) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
          let Just targetRef@(EdictReference targetIdx) = edict^.eTargetEnt
          Just target <- preuse $ gameBaseGlobals.gbGEdicts.ix targetIdx

          if isJust $ target^.eEdictInfo.eiPathTarget
            then do
              let saveTarget = target^.eEdictInfo.eiTarget
              gameBaseGlobals.gbGEdicts.ix targetIdx.eEdictInfo.eiTarget .= (target^.eEdictInfo.eiPathTarget)

              GameUtil.useTargets targetRef (edict^.eEdictOther.eoActivator)

              gameBaseGlobals.gbGEdicts.ix targetIdx.eEdictInfo.eiTarget .= saveTarget

              -- make sure we didn't get killed by a killtarget
              Just inUse <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eInUse
              return $ if inUse then False else True

            else return False

moveCalc :: EdictReference -> V3 Float -> EntThink -> Quake ()
moveCalc er@(EdictReference edictIdx) dest func = do
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity .= V3 0 0 0

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    let dir = dest - (edict^.eEntityState.esOrigin)

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo) $ do
      miDir .= normalize dir
      miRemainingDistance .= sqrt (quadrance dir) -- TODO: make sure we are correct here
      miEndFunc .= Just func

    time <- use $ gameBaseGlobals.gbLevel.llTime

    if (edict^.eMoveInfo.miSpeed) == (edict^.eMoveInfo.miAccel) && (edict^.eMoveInfo.miSpeed) == (edict^.eMoveInfo.miDecel)
      then do
        currentEntity <- use $ gameBaseGlobals.gbLevel.llCurrentEntity

        let comparedEntity = if (edict^.eFlags) .&. Constants.flTeamSlave /= 0
                               then edict^.eEdictOther.eoTeamMaster
                               else Just er

        if currentEntity == comparedEntity
          then void $ think moveBegin er
          else
            zoom (gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction) $ do
              eaNextThink .= time + Constants.frameTime
              eaThink .= Just moveBegin
      else do
        -- aceelerative
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eMoveInfo.miCurrentSpeed .= 0
          eEdictAction.eaThink .= Just thinkAccelMove
          eEdictAction.eaNextThink .= time + Constants.frameTime

angleMoveCalc :: EdictReference -> EntThink -> Quake ()
angleMoveCalc _ _ = do
    io (putStrLn "GameFunc.angleMoveCalc") >> undefined -- TODO

moveBegin :: EntThink
moveBegin =
  GenericEntThink "move_begin" $ \edictRef@(EdictReference edictIdx) -> do
    Just moveInfo <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo

    if (moveInfo^.miSpeed) * Constants.frameTime >= (moveInfo^.miRemainingDistance)
      then do
        void $ think moveFinal edictRef
        return True
      else do
        let velocity = fmap (* (moveInfo^.miSpeed)) (moveInfo^.miDir)
            frames :: Int = floor $ ((moveInfo^.miRemainingDistance) / (moveInfo^.miSpeed)) / Constants.frameTime
            framesF :: Float = fromIntegral frames

        time <- use $ gameBaseGlobals.gbLevel.llTime

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEdictPhysics.eVelocity .= velocity
          eMoveInfo.miRemainingDistance -= framesF * (moveInfo^.miSpeed) * Constants.frameTime
          eEdictAction.eaNextThink .= time + (framesF * Constants.frameTime)
          eEdictAction.eaThink .= Just moveFinal

        return True

thinkAccelMove :: EntThink
thinkAccelMove =
  GenericEntThink "think_accelmove" $ \_ -> do
    io (putStrLn "GameFunc.thinkAccelMove") >> undefined -- TODO

moveFinal :: EntThink
moveFinal =
  GenericEntThink "move_final" $ \edictRef@(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    if (edict^.eMoveInfo.miRemainingDistance) == 0
      then do
        void $ think moveDone edictRef
        return True
      else do
        let velocity = fmap (* ((edict^.eMoveInfo.miRemainingDistance) / Constants.frameTime)) (edict^.eMoveInfo.miDir)
        time <- use $ gameBaseGlobals.gbLevel.llTime
        
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEdictPhysics.eVelocity .= velocity
          eEdictAction.eaThink .= Just moveDone
          eEdictAction.eaNextThink .= time + Constants.frameTime

        return True

moveDone :: EntThink
moveDone =
  GenericEntThink "move_done" $ \edictRef@(EdictReference edictIdx) -> do
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictPhysics.eVelocity .= V3 0 0 0
    Just endFunc <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miEndFunc

    void $ think (fromJust endFunc) edictRef

    return True

rotatingUse :: EntUse
rotatingUse =
  GenericEntUse "rotating_use" $ \(EdictReference selfIdx) _ _ -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    vec3origin <- use $ globals.vec3Origin

    if (self^.eEdictPhysics.eAVelocity) /= vec3origin
      then
        zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
          eEntityState.esSound .= 0
          eEdictPhysics.eAVelocity .= V3 0 0 0
          eEdictAction.eaTouch .= Nothing
      else do
        zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
          eEntityState.esSound .= (self^.eMoveInfo.miSoundMiddle)
          eEdictPhysics.eAVelocity .= fmap (* (self^.eEdictPhysics.eSpeed)) (self^.eEdictPhysics.eMoveDir)

        when ((self^.eSpawnFlags) .&. 16 /= 0) $
          gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictAction.eaTouch .= Just rotatingTouch

rotatingBlocked :: EntBlocked
rotatingBlocked =
  GenericEntBlocked "rotating_blocked" $ \_ _ -> do
    io (putStrLn "GameFunc.rotatingBlocked") >> undefined -- TODO

rotatingTouch :: EntTouch
rotatingTouch =
  GenericEntTouch "rotating_touch" $ \_ _ _ _ -> do
    io (putStrLn "GameFunc.rotatingTouch") >> undefined -- TODO

doorGoUp :: EdictReference -> Maybe EdictReference -> Quake ()
doorGoUp selfRef@(EdictReference selfIdx) activatorRef = do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    unless ((self^.eMoveInfo.miState) == Constants.stateUp) $ do
      if (self^.eMoveInfo.miState) == Constants.stateTop
        then do
          -- reset top wait time
          when ((self^.eMoveInfo.miWait) >= 0) $ do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime
            gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictAction.eaNextThink .= levelTime + (self^.eMoveInfo.miWait)
        else do
          when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
            when ((self^.eMoveInfo.miSoundStart) /= 0) $ do
              sound <- use $ gameBaseGlobals.gbGameImport.giSound
              sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0
            gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esSound .= self^.eMoveInfo.miSoundMiddle

          gameBaseGlobals.gbGEdicts.ix selfIdx.eMoveInfo.miState .= Constants.stateUp

          if | (self^.eClassName) == "func_door" -> moveCalc selfRef (self^.eMoveInfo.miEndOrigin) doorHitTop
             | (self^.eClassName) == "func_door_rotating" -> angleMoveCalc selfRef doorHitTop
             | otherwise -> return ()

          GameUtil.useTargets selfRef activatorRef
          doorUseAreaPortals selfRef True

doorHitTop :: EntThink
doorHitTop =
  GenericEntThink "door_hit_top" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((self^.eMoveInfo.miSoundEnd) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0

      gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esSound .= 0

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMoveInfo.miState .= Constants.stateTop

    if (self^.eSpawnFlags) .&. Constants.doorToggle /= 0
      then
        return True
      else do
        when ((self^.eMoveInfo.miWait) >= 0) $ do
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime
          zoom (gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictAction) $ do
            eaThink .= Just doorGoDown
            eaNextThink .= levelTime + (self^.eMoveInfo.miWait)

        return True

doorGoDown :: EntThink
doorGoDown =
  GenericEntThink "door_go_down" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((self^.eMoveInfo.miSoundStart) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0
      gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esSound .= self^.eMoveInfo.miSoundMiddle

    when ((self^.eEdictStatus.eMaxHealth) /= 0) $ do
      zoom (gameBaseGlobals.gbGEdicts.ix selfIdx.eEdictStatus) $ do
        eTakeDamage .= Constants.damageYes
        eHealth .= self^.eEdictStatus.eMaxHealth

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMoveInfo.miState .= Constants.stateDown

    if | (self^.eClassName) == "func_door" -> moveCalc selfRef (self^.eMoveInfo.miStartOrigin) doorHitBottom
       | (self^.eClassName) == "func_door_rotating" -> angleMoveCalc selfRef doorHitBottom
       | otherwise -> return ()

    return True

doorHitBottom :: EntThink
doorHitBottom =
  GenericEntThink "door_hit_bottom" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((self^.eMoveInfo.miSoundEnd) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0
      gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esSound .= 0

    gameBaseGlobals.gbGEdicts.ix selfIdx.eMoveInfo.miState .= Constants.stateBottom
    doorUseAreaPortals selfRef False
    return True

{-
- QUAKED func_conveyor (0 .5 .8) ? START_ON TOGGLE Conveyors are stationary
- brushes that move what's on them. The brush should be have a surface with
- at least one current content enabled. speed default 100
-}
funcConveyorUse :: EntUse
funcConveyorUse =
  GenericEntUse "func_conveyor_use" $ \(EdictReference selfIdx) _ _ -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    if (self^.eSpawnFlags) .&. 1 /= 0
      then do
        zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
          eEdictPhysics.eSpeed .= 0
          eSpawnFlags %= (.&. (complement 1))
      else do
        zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
          eEdictPhysics.eSpeed .= fromIntegral (self^.eCount)
          eSpawnFlags %= (.|. 1)

    Just spawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eSpawnFlags
    when (spawnFlags .&. 2 == 0) $
      gameBaseGlobals.gbGEdicts.ix selfIdx.eCount .= 0
