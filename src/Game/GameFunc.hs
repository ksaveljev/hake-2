{-# LANGUAGE OverloadedStrings #-}
module Game.GameFunc where

import Control.Lens (use, preuse, (.=), (^.), ix, zoom, (%=))
import Control.Monad (when, liftM, void)
import Data.Bits ((.&.), (.|.))
import Data.Maybe (isJust, fromJust, isNothing)
import Linear (V3(..), _x, _y, _z)
import qualified Data.ByteString as B

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameBase as GameBase
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

platLowTrigger :: Int
platLowTrigger = 1

stateTop :: Int
stateTop = 0

stateBottom :: Int
stateBottom = 1

stateUp :: Int
stateUp = 2

stateDown :: Int
stateDown = 3

doorStartOpen :: Int
doorStartOpen = 1

doorReverse :: Int
doorReverse = 2

doorCrusher :: Int
doorCrusher = 4

doorNoMonster :: Int
doorNoMonster = 8

doorToggle :: Int
doorToggle = 32

doorXAxis :: Int
doorXAxis = 64

doorYAxis :: Int
doorYAxis = 128

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
      soundIndex "switches/butn2.wav" >>= (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miSoundStart .=)

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
      miState .= stateBottom
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
      soundIndex "doors/dr1_strt.wav" >>= (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miSoundStart .=)
      soundIndex "doors/dr1_mid.wav" >>= (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miSoundMiddle .=)
      soundIndex "doors/dr1_end.wav" >>= (gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miSoundEnd .=)

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
    when ((edict^.eSpawnFlags) .&. doorStartOpen /= 0) $ do
      zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
        eEntityState.esOrigin .= pos2
        eEdictPhysics.ePos2 .= origin
        eEdictPhysics.ePos1 .= pos2

    gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveInfo.miState .= stateBottom

    if (edict^.eEdictStatus.eHealth) /= 0
      then
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEdictStatus.eTakeDamage .= Constants.damageYes
          eEdictStatus.eMaxHealth .= (edict^.eEdictStatus.eHealth)
          eEdictAction.eaDie .= Just doorKilled
      else
        when (isJust (edict^.eEdictInfo.eiTargetName) && isJust (edict^.eEdictInfo.eiMessage)) $ do
          void $ soundIndex "misc/talk.wav"
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
  GenericEntThink "sp_func_conveyor" $ \_ -> do
    io (putStrLn "GameFunc.spFuncConveyor") >> undefined -- TODO

spFuncKillBox :: EntThink
spFuncKillBox =
  GenericEntThink "sp_func_killbox" $ \_ -> do
    io (putStrLn "GameFunc.spFuncKillBox") >> undefined -- TODO

spFuncRotating :: EntThink
spFuncRotating =
  GenericEntThink "sp_func_rotating" $ \_ -> do
    io (putStrLn "GameFunc.spFuncRotating") >> undefined -- TODO

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
      noiseIdx <- soundIndex (fromJust noise)
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

funcTimerThink :: EntThink
funcTimerThink =
  GenericEntThink "func_timer_think" $ \_ -> do
    io (putStrLn "GameFunc.funcTimerThink") >> undefined -- TODO

trainBlocked :: EntBlocked
trainBlocked =
  GenericEntBlocked "train_blocked" $ \_ _ -> do
    io (putStrLn "GameFunc.trainBlocked") >> undefined -- TODO

trainUse :: EntUse
trainUse =
  GenericEntUse "train_use" $ \_ _ _ -> do
    io (putStrLn "GameFunc.trainUse") >> undefined -- TODO

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
        let target = fromJust (edict^.eEdictInfo.eiTarget)
        entRef <- GameBase.pickTarget target

        if isNothing entRef
          then
            dprintf $ "train_find: target " `B.append` target `B.append` " not found\n"
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
  GenericEntUse "door_use" $ \_ _ _ -> do
    io (putStrLn "GameFunc.doorUse") >> undefined -- TODO

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
  GenericEntThink "think_calc_movespeed" $ \_ -> do
    io (putStrLn "GameFunc.thinkCalcMoveSpeed") >> undefined -- TODO

thinkSpawnDoorTrigger :: EntThink
thinkSpawnDoorTrigger =
  GenericEntThink "think_spawn_door_trigger" $ \_ -> do
    io (putStrLn "GameFunc.thinkSpawnDoorTrigger") >> undefined -- TODO

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
  GenericEntThink "train_next" $ \_ -> do
    io (putStrLn "GameFunc.trainNext") >> undefined -- TODO
