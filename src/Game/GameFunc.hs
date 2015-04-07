{-# LANGUAGE OverloadedStrings #-}
module Game.GameFunc where

import Control.Lens (use, preuse, (.=), (^.), ix, zoom)
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.Maybe (isJust)
import Linear (V3(..))
import qualified Data.ByteString as B

import Quake
import QuakeState
import Game.Adapters
import qualified Constants
import qualified Util.Lib as Lib

trainStartOn :: Int
trainStartOn = 1

trainToggle :: Int
trainToggle = 2

trainBlockStops :: Int
trainBlockStops = 4

spFuncButton :: EntThink
spFuncButton =
  GenericEntThink "sp_func_button" $ \_ -> do
    io (putStrLn "GameFunc.spFuncButton") >> undefined -- TODO

spFuncDoor :: EntThink
spFuncDoor =
  GenericEntThink "sp_func_door" $ \_ -> do
    io (putStrLn "GameFunc.spFuncDoor") >> undefined -- TODO

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
    -- TODO: if (GameBase.st.noise != null)
    {-
    when (isJust noise) $ do
    -}
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
  GenericEntThink "func_train_find" $ \_ -> do
    io (putStrLn "GameFunc.funcTrainFind") >> undefined -- TODO
