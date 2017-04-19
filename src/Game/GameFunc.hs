module Game.GameFunc
    ( funcTrainFind
    , spFuncButton
    , spFuncConveyor
    , spFuncDoor
    , spFuncDoorRotating
    , spFuncDoorSecret
    , spFuncKillBox
    , spFuncPlat
    , spFuncRotating
    , spFuncTimer
    , spFuncTrain
    , spFuncWater
    , spTriggerElevator
    , trainUse
    ) where

import           Control.Lens      (use, (^.), (%=), (&), (.~), (%~))
import           Control.Monad     (unless, when, void)
import           Data.Bits         (complement, (.|.), (.&.))
import qualified Data.ByteString   as B
import           Data.Maybe        (isJust, isNothing)
import           Linear            (V3(..), _x, _y, _z)

import qualified Constants
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameBase     as GameBase
import qualified Game.GameUtil     as GameUtil
import           Game.LevelLocalsT
import           Game.MoveInfoT
import           Game.SpawnTempT
import qualified QCommon.Com       as Com
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib          as Lib

import {-# SOURCE #-} Game.GameImportT

trainStartOn :: Int
trainStartOn = 1

trainToggle :: Int
trainToggle = 2

trainBlockStops :: Int
trainBlockStops = 4

secretAlwaysShoot :: Int
secretAlwaysShoot = 1

secretFirstLeft :: Int
secretFirstLeft = 2

secretFirstDown :: Int
secretFirstDown = 4

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
doorToggle = 31

doorXAxis :: Int
doorXAxis = 64

doorYAxis :: Int
doorYAxis = 128

spFuncButton :: EntThink
spFuncButton = EntThink "sp_func_button" $ \edictRef -> do
    GameBase.setMoveDir edictRef =<< readRef edictRef
    edict <- readRef edictRef
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeStop
                                & eSolid .~ Constants.solidBsp)
    setModel <- use (gameBaseGlobals.gbGameImport.giSetModel)
    setModel edictRef (edict^.eiModel)
    when ((edict^.eSounds) /= 1) $ do
        soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
        soundIdx <- soundIndex (Just "switches/butn2.wav")
        modifyRef edictRef (\v -> v & eMoveInfo.miSoundStart .~ soundIdx)
    let speed = if (edict^.eSpeed) == 0 then 40 else edict^.eSpeed
    modifyRef edictRef (\v -> v & eSpeed .~ speed
                                & eAccel %~ (\n -> if n == 0 then speed else n)
                                & eDecel %~ (\n -> if n == 0 then speed else n)
                                & eWait %~ (\n -> if n == 0 then 3 else n)
                                & ePos1 .~ (edict^.eEntityState.esOrigin))
    gameBaseGlobals.gbSpawnTemp.stLip %= (\v -> if v == 0 then 4 else v)
    lip <- use (gameBaseGlobals.gbSpawnTemp.stLip)
    let moveDir = edict^.eMoveDir
        absMoveDir = fmap abs moveDir
        size = edict^.eSize
        dist = (absMoveDir^._x) * (size^._x)
             + (absMoveDir^._y) * (size^._y)
             + (absMoveDir^._z) * (size^._z)
             - (fromIntegral lip)
        pos2 = (edict^.eEntityState.esOrigin) + fmap (* dist) moveDir
    modifyRef edictRef (\v -> v & ePos2 .~ pos2
                                & eUse .~ Just buttonUse
                                & eEntityState.esEffects %~ (.|. Constants.efAnim01))
    checkHealth edictRef edict
    updatedEdict <- readRef edictRef
    modifyRef edictRef (\v -> v & eMoveInfo.miState .~ Constants.stateBottom
                                & eMoveInfo.miSpeed .~ (updatedEdict^.eSpeed)
                                & eMoveInfo.miAccel .~ (updatedEdict^.eAccel)
                                & eMoveInfo.miDecel .~ (updatedEdict^.eDecel)
                                & eMoveInfo.miWait .~ (updatedEdict^.eWait)
                                & eMoveInfo.miStartOrigin .~ (updatedEdict^.ePos1)
                                & eMoveInfo.miStartAngles .~ (updatedEdict^.eEntityState.esAngles)
                                & eMoveInfo.miEndOrigin .~ (updatedEdict^.ePos2)
                                & eMoveInfo.miEndAngles .~ (updatedEdict^.eEntityState.esAngles))
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity edictRef
    return True
  where
    checkHealth edictRef edict
        | (edict^.eHealth) /= 0 =
            modifyRef edictRef (\v -> v & eMaxHealth .~ (edict^.eHealth)
                                        & eDie .~ Just buttonKilled
                                        & eTakeDamage .~ Constants.damageYes)
        | isNothing (edict^.eTargetName) =
            modifyRef edictRef (\v -> v & eTouch .~ Just buttonTouch)
        | otherwise =
            return ()

buttonUse :: EntUse
buttonUse = EntUse "button_use" $ \selfRef _ activatorRef -> do
    modifyRef selfRef (\v -> v & eActivator .~ activatorRef)
    void (entThink buttonFire selfRef)

buttonKilled :: EntDie
buttonKilled = EntDie "button_killed" $ \selfRef _ attackerRef _ _ -> do
    self <- readRef selfRef
    modifyRef selfRef (\v -> v & eActivator .~ Just attackerRef
                               & eHealth .~ (self^.eMaxHealth)
                               & eTakeDamage .~ Constants.damageNo)
    void (entThink buttonFire selfRef)

buttonTouch :: EntTouch
buttonTouch = EntTouch "button_touch" $ \selfRef otherRef _ _ -> do
    other <- readRef otherRef
    unless (isNothing (other^.eClient) || (other^.eHealth) <= 0) $ do
        modifyRef selfRef (\v -> v & eActivator .~ Just otherRef)
        void (entThink buttonFire selfRef)

buttonFire :: EntThink
buttonFire = EntThink "button_fire" $ \selfRef -> do
    self <- readRef selfRef
    doButtonFire selfRef self
  where
    doButtonFire selfRef self
        | (self^.eMoveInfo.miState) == stateUp || (self^.eMoveInfo.miState) == stateTop =
            return True
        | otherwise = do
            modifyRef selfRef (\v -> v & eMoveInfo.miState .~ stateUp)
            when ((self^.eMoveInfo.miSoundStart) /= 0 && (self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
                sound <- use (gameBaseGlobals.gbGameImport.giSound)
                sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0
            moveCalc selfRef (self^.eMoveInfo.miEndOrigin) buttonWait
            return True

buttonWait :: EntThink
buttonWait = EntThink "button_wait" $ \selfRef -> do
    self <- readRef selfRef
    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ stateTop
                               & eEntityState.esEffects %~ (.&. (complement Constants.efAnim01))
                               & eEntityState.esEffects %~ (.|. Constants.efAnim23))
    GameUtil.useTargets selfRef (self^.eActivator)
    modifyRef selfRef (\v -> v & eEntityState.esFrame .~ 1)
    self' <- readRef selfRef
    when ((self'^.eMoveInfo.miWait) >= 0) $ do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef selfRef (\v -> v & eNextThink .~ levelTime + (self'^.eMoveInfo.miWait)
                                   & eThink .~ Just buttonReturn)
    return True

buttonReturn :: EntThink
buttonReturn = EntThink "button_return" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ stateDown)
    self <- readRef selfRef
    moveCalc selfRef (self^.eMoveInfo.miStartOrigin) buttonDone
    modifyRef selfRef (\v -> v & eEntityState.esFrame .~ 0)
    self' <- readRef selfRef
    when ((self'^.eHealth) /= 0) $
        modifyRef selfRef (\v -> v & eTakeDamage .~ Constants.damageYes)
    return True

buttonDone :: EntThink
buttonDone = EntThink "button_done" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ stateBottom
                               & eEntityState.esEffects %~ (.&. (complement Constants.efAnim23))
                               & eEntityState.esEffects %~ (.|. Constants.efAnim01))
    return True

spFuncConveyor :: EntThink
spFuncConveyor = EntThink "sp_func_conveyor" $ \selfRef -> do
    self <- readRef selfRef
    let speed = if (self^.eSpeed) == 0 then 100 else self^.eSpeed
    modifyRef selfRef (\v -> v & eSpeed .~ speed)
    when ((self^.eSpawnFlags) .&. 1 == 0) $ do
        modifyRef selfRef (\v -> v & eCount .~ truncate speed
                                   & eSpeed .~ 0)
    modifyRef selfRef (\v -> v & eUse .~ Just funcConveyorUse)
    setModel <- use (gameBaseGlobals.gbGameImport.giSetModel)
    setModel selfRef (self^.eiModel)
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidBsp)
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity selfRef
    return True

funcConveyorUse :: EntUse
funcConveyorUse = EntUse "func_conveyor_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    conveyorUse selfRef self
    self' <- readRef selfRef
    when ((self'^.eSpawnFlags) .&. 2 == 0) $
        modifyRef selfRef (\v -> v & eCount .~ 0)
  where
    conveyorUse selfRef self
        | (self^.eSpawnFlags) .&. 1 /= 0 =
            modifyRef selfRef (\v -> v & eSpeed .~ 0
                                       & eSpawnFlags %~ (.&. (complement 1)))
        | otherwise =
            modifyRef selfRef (\v -> v & eSpeed .~ fromIntegral (self^.eCount)
                                       & eSpawnFlags %~ (.|. 1))

spFuncDoor :: EntThink
spFuncDoor = error "GameFunc.spFuncDoor" -- TODO

spFuncDoorSecret :: EntThink
spFuncDoorSecret = error "GameFunc.spFuncDoorSecret" -- TODO

spFuncDoorRotating :: EntThink
spFuncDoorRotating = error "GameFunc.spFuncDoorRotating" -- TODO

spFuncKillBox :: EntThink
spFuncKillBox = error "GameFunc.spFuncKillBox" -- TODO

spFuncPlat :: Ref EdictT -> Quake ()
spFuncPlat = error "GameFunc.spFuncPlat" -- TODO

spFuncRotating :: EntThink
spFuncRotating = error "GameFunc.spFuncRotating" -- TODO

spFuncTimer :: Ref EdictT -> Quake ()
spFuncTimer edictRef = do
    edict <- readRef edictRef
    when ((edict^.eWait) == 0) $
        modifyRef edictRef (\v -> v & eWait .~ 1)
    modifyRef edictRef (\v -> v & eUse .~ Just funcTimerUse
                                & eThink .~ Just funcTimerThink)
    when ((edict^.eRandom) >= (edict^.eWait)) $ do
        modifyRef edictRef (\v -> v & eRandom .~ (edict^.eWait) - Constants.frameTime)
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["func_timer at ", Lib.vtos (edict^.eEntityState.esOrigin), " has random >= wait\n"])
    when (((edict^.eSpawnFlags) .&. 1) /= 0) $ do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        pauseTime <- use (gameBaseGlobals.gbSpawnTemp.stPauseTime)
        cr <- Lib.crandom
        modifyRef edictRef (\v -> v & eNextThink .~ levelTime + 1 + pauseTime + (edict^.eDelay) + (edict^.eWait) + cr * (edict^.eRandom)
                                    & eActivator .~ Just edictRef)
    modifyRef edictRef (\v -> v & eSvFlags .~ Constants.svfNoClient)

funcTimerUse :: EntUse
funcTimerUse = error "GameFunc.funcTimerUse" -- TODO

funcTimerThink :: EntThink
funcTimerThink = EntThink "func_timer_think" $ \edictRef -> do
    edict <- readRef edictRef
    GameUtil.useTargets edictRef (edict^.eActivator)
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    r <- Lib.crandom
    modifyRef edictRef (\v -> v & eNextThink .~ levelTime + (edict^.eWait) + r * (edict^.eRandom))
    return True

spFuncTrain :: Ref EdictT -> Quake ()
spFuncTrain = error "GameFunc.spFuncTrain" -- TODO

spFuncWater :: Ref EdictT -> Quake ()
spFuncWater = error "GameFunc.spFuncWater" -- TODO

spTriggerElevator :: EntThink
spTriggerElevator = error "GameFunc.spTriggerElevator" -- TODO

moveCalc :: Ref EdictT -> V3 Float -> EntThink -> Quake ()
moveCalc = error "GameFunc.moveCalc" -- TODO

trainUse :: EntUse
trainUse = EntUse "train_use" $ \selfRef _ activatorRef -> do
    modifyRef selfRef (\v -> v & eActivator .~ activatorRef)
    self <- readRef selfRef
    doTrainUse selfRef self
  where
    doTrainUse selfRef self
        | (self^.eSpawnFlags) .&. trainStartOn /= 0 =
            unless ((self^.eSpawnFlags) .&. trainToggle == 0) $ do
                modifyRef selfRef (\v -> v & eSpawnFlags %~ (.&. (complement trainStartOn))
                                           & eVelocity .~ V3 0 0 0
                                           & eNextThink .~ 0)
        | isJust (self^.eTargetEnt) = trainResume selfRef
        | otherwise = void (entThink trainNext selfRef)

trainResume :: Ref EdictT -> Quake ()
trainResume selfRef = do
    self <- readRef selfRef
    maybe targetEntError (doTrainResume self) (self^.eTargetEnt)
  where
    targetEntError = Com.fatalError "GameFunc.trainResume self^.eTargetEnt is Nothing"
    doTrainResume self edictRef = do
        edict <- readRef edictRef
        let dest = (edict^.eEntityState.esOrigin) - (self^.eMins)
        modifyRef selfRef (\v -> v & eMoveInfo.miState .~ stateTop
                                   & eMoveInfo.miStartOrigin .~ (self^.eEntityState.esOrigin)
                                   & eMoveInfo.miEndOrigin .~ dest)
        moveCalc selfRef dest trainWait
        modifyRef selfRef (\v -> v & eSpawnFlags %~ (.|. trainStartOn))

trainNext :: EntThink
trainNext = error "GameFunc.trainNext" -- TODO

trainWait :: EntThink
trainWait = error "GameFunc.trainWait" -- TODO

funcTrainFind :: EntThink
funcTrainFind = error "GameFunc.funcTrainFind" -- TODO
