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

import           Control.Lens          (use, (^.), (.=), (%=), (&), (.~), (%~), (-~))
import           Control.Monad         (unless, when, void)
import           Data.Bits             (complement, (.|.), (.&.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (toLower)
import           Data.Maybe            (isJust, isNothing)
import           Linear                (V3(..), dot, norm, normalize, _x, _y, _z)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameBase         as GameBase
import qualified Game.GameCombat       as GameCombat
import qualified Game.GameMisc         as GameMisc
import qualified Game.GameUtil         as GameUtil
import           Game.LevelLocalsT
import           Game.MoveInfoT
import           Game.SpawnTempT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import qualified Util.Lib              as Lib
import qualified Util.Math3D           as Math3D

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
    conveyorUse selfRef =<< readRef selfRef
    self <- readRef selfRef
    when ((self^.eSpawnFlags) .&. 2 == 0) $
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
spFuncDoor = EntThink "sp_func_door" $ \edictRef -> do
    GameBase.setMoveDir edictRef =<< readRef edictRef
    edict <- readRef edictRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    when ((edict^.eSounds) /= 1) $ do
        soundStart <- (gameImport^.giSoundIndex) (Just "doors/dr1_strt.wav")
        soundMiddle <- (gameImport^.giSoundIndex) (Just "doors/dr1_mid.wav")
        soundEnd <- (gameImport^.giSoundIndex) (Just "doors/dr1_end.wav")
        modifyRef edictRef (\v -> v & eMoveInfo.miSoundStart .~ soundStart
                                    & eMoveInfo.miSoundMiddle .~ soundMiddle
                                    & eMoveInfo.miSoundEnd .~ soundEnd)
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                & eSolid .~ Constants.solidBsp)
    (gameImport^.giSetModel) edictRef (edict^.eiModel)
    modifyRef edictRef (\v -> v & eBlocked .~ Just doorBlocked
                                & eUse .~ Just doorUse)
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    let speed = (edictSpeed (edict^.eSpeed)) * (deathmatchMultiplier deathmatch)
    modifyRef edictRef (\v -> v & eSpeed .~ speed)
    when ((edict^.eAccel) == 0) $
        modifyRef edictRef (\v -> v & eAccel .~ speed)
    when ((edict^.eDecel) == 0) $
        modifyRef edictRef (\v -> v & eDecel .~ speed)
    when ((edict^.eWait) == 0) $
        modifyRef edictRef (\v -> v & eWait .~ 3)
    when ((edict^.eDmg) == 0) $
        modifyRef edictRef (\v -> v & eDmg .~ 2)
    checkLip
    -- calculate second position
    lip <- use (gameBaseGlobals.gbSpawnTemp.stLip)
    let moveDir = edict^.eMoveDir
        absMoveDir = fmap abs moveDir
        size = edict^.eSize
        dist = (absMoveDir^._x) * (size^._x)
             + (absMoveDir^._y) * (size^._y)
             + (absMoveDir^._z) * (size^._z)
             - (fromIntegral lip)
        pos2 = (edict^.eEntityState.esOrigin) + fmap (* dist) moveDir
    modifyRef edictRef (\v -> v & ePos1 .~ (edict^.eEntityState.esOrigin)
                                & eMoveInfo.miDistance .~ dist
                                & ePos2 .~ pos2
                                & eMoveInfo.miState .~ Constants.stateBottom)
    -- if it starts open, switch the positions
    when ((edict^.eSpawnFlags) .&. Constants.doorStartOpen /= 0) $ do
        modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ pos2
                                    & ePos2 .~ (edict^.eEntityState.esOrigin)
                                    & ePos1 .~ pos2)
    updateEdict edictRef =<< readRef edictRef
    updatedEdict <- readRef edictRef
    modifyRef edictRef (\v -> v & eMoveInfo.miSpeed .~ (updatedEdict^.eSpeed)
                                & eMoveInfo.miAccel .~ (updatedEdict^.eAccel)
                                & eMoveInfo.miDecel .~ (updatedEdict^.eDecel)
                                & eMoveInfo.miWait .~ (updatedEdict^.eWait)
                                & eMoveInfo.miStartOrigin .~ (updatedEdict^.ePos1)
                                & eMoveInfo.miStartAngles .~ (updatedEdict^.eEntityState.esAngles)
                                & eMoveInfo.miEndOrigin .~ (updatedEdict^.ePos2)
                                & eMoveInfo.miEndAngles .~ (updatedEdict^.eEntityState.esAngles))
    when ((updatedEdict^.eSpawnFlags) .&. 16 /= 0) $
        modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAll))
    when ((updatedEdict^.eSpawnFlags) .&. 64 /= 0) $
        modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAllFast))
    -- to simplify logic elsewhere, make non-teamed doors into a team of one
    when (isNothing (updatedEdict^.eTeam)) $
        modifyRef edictRef (\v -> v & eTeamMaster .~ Just edictRef)
    (gameImport^.giLinkEntity) edictRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef edictRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime)
    modifyRef edictRef (\v -> v & eThink .~ Just (nextThink updatedEdict))
    return True
  where
    edictSpeed speed
        | speed == 0 = 100
        | otherwise = speed
    deathmatchMultiplier deathmatch
        | deathmatch /= 0 = 2
        | otherwise = 1
    checkLip = do
        lip <- use (gameBaseGlobals.gbSpawnTemp.stLip)
        when (lip == 0) $
            gameBaseGlobals.gbSpawnTemp.stLip .= 8
    updateEdict edictRef edict
        | (edict^.eHealth) /= 0 =
            modifyRef edictRef (\v -> v & eTakeDamage .~ Constants.damageYes
                                        & eMaxHealth .~ (edict^.eHealth)
                                        & eDie .~ Just doorKilled)
        | otherwise =
            when (isJust (edict^.eTargetName) && isJust (edict^.eMessage)) $ do
                soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
                void (soundIndex (Just "misc/talk.wav"))
                modifyRef edictRef (\v -> v & eTouch .~ Just doorTouch)
    nextThink edict
        | (edict^.eHealth) /= 0 || isJust (edict^.eTargetName) = thinkCalcMoveSpeed
        | otherwise = thinkSpawnDoorTrigger

spFuncDoorSecret :: EntThink
spFuncDoorSecret = EntThink "sp_func_door_secret" $ \edictRef -> do
    gameImport <- use (gameBaseGlobals.gbGameImport)
    soundStart <- (gameImport^.giSoundIndex) (Just "doors/dr1_strt.wav")
    soundMiddle <- (gameImport^.giSoundIndex) (Just "doors/dr1_mid.wav")
    soundEnd <- (gameImport^.giSoundIndex) (Just "doors/dr1_end.wav")
    modifyRef edictRef (\v -> v & eMoveInfo.miSoundStart .~ soundStart
                                & eMoveInfo.miSoundMiddle .~ soundMiddle
                                & eMoveInfo.miSoundEnd .~ soundEnd
                                & eMoveType .~ Constants.moveTypePush
                                & eSolid .~ Constants.solidBsp
                                & eBlocked .~ Just doorSecretBlocked
                                & eUse .~ Just doorSecretUse)
    edict <- readRef edictRef
    (gameImport^.giSetModel) edictRef (edict^.eiModel)
    when (isNothing (edict^.eTargetName) || (edict^.eSpawnFlags) .&. secretAlwaysShoot /= 0) $
        modifyRef edictRef (\v -> v & eHealth .~ 0
                                    & eTakeDamage .~ Constants.damageYes
                                    & eDie .~ Just doorSecretDie)
    when ((edict^.eDmg) == 0) $
        modifyRef edictRef (\v -> v & eDmg .~ 2)
    when ((edict^.eWait) == 0) $
        modifyRef edictRef (\v -> v & eWait .~ 5)
    modifyRef edictRef (\v -> v & eMoveInfo.miAccel .~ 50
                                & eMoveInfo.miDecel .~ 50
                                & eMoveInfo.miSpeed .~ 50)
    let (forward, right, up) = Math3D.angleVectors (edict^.eEntityState.esAngles) True True True
        side = 1.0 - fromIntegral ((edict^.eSpawnFlags) .&. secretFirstLeft)
        width = if (edict^.eSpawnFlags) .&. secretFirstDown /= 0
                    then abs (up `dot` (edict^.eSize))
                    else abs (right `dot` (edict^.eSize))
        len = abs (forward `dot` (edict^.eSize))
        pos1 = if (edict^.eSpawnFlags) .&. secretFirstDown /= 0
                   then (edict^.eEntityState.esOrigin) + fmap (* ((-1) * width)) up
                   else (edict^.eEntityState.esOrigin) + fmap (* (side * width)) right
        pos2 = pos1 + fmap (* len) forward
    modifyRef edictRef (\v -> v & eEntityState.esAngles .~ V3 0 0 0
                                & ePos1 .~ pos1
                                & ePos2 .~ pos2)
    updateEdict edictRef =<< readRef edictRef
    modifyRef edictRef (\v -> v & eClassName .~ "func_door")
    (gameImport^.giLinkEntity) edictRef
    return True
  where
    updateEdict edictRef edict
        | (edict^.eHealth) /= 0 =
            modifyRef edictRef (\v -> v & eTakeDamage .~ Constants.damageYes
                                        & eDie .~ Just doorKilled
                                        & eMaxHealth .~ (edict^.eHealth))
        | otherwise = do
            soundIndex <- use (gameBaseGlobals.gbGameImport.giSoundIndex)
            void (soundIndex (Just "misc/talk.wav"))
            modifyRef edictRef (\v -> v & eTouch .~ Just doorTouch)

spFuncDoorRotating :: EntThink
spFuncDoorRotating = error "GameFunc.spFuncDoorRotating" -- TODO

spFuncKillBox :: EntThink
spFuncKillBox = error "GameFunc.spFuncKillBox" -- TODO

spFuncPlat :: Ref EdictT -> Quake ()
spFuncPlat edictRef = do
    edict <- readRef edictRef
    gameImport <- use (gameBaseGlobals.gbGameImport)
    modifyRef edictRef (\v -> v & eEntityState.esAngles .~ V3 0 0 0
                                & eSolid .~ Constants.solidBsp
                                & eMoveType .~ Constants.moveTypePush
                                & eBlocked .~ Just platBlocked
                                & eSpeed %~ (\s -> if s == 0 then 20 else s * 0.1)
                                & eAccel %~ (\a -> if a == 0 then 5 else a * 0.1)
                                & eDecel %~ (\d -> if d == 0 then 5 else d * 0.1)
                                & eDmg %~ (\d -> if d == 0 then 2 else d))
    checkLip
    (gameImport^.giSetModel) edictRef (edict^.eiModel)
    spawnTemp <- use (gameBaseGlobals.gbSpawnTemp)
    -- pos1 is the top position pos2 is the bottom
    let pos1 = edict^.eEntityState.esOrigin
        pos2 | (spawnTemp^.stHeight) /= 0 = (edict^.eEntityState.esOrigin) & _z -~ fromIntegral (spawnTemp^.stHeight)
             | otherwise                  = (edict^.eEntityState.esOrigin) & _z -~ (edict^.eMaxs._z) - (edict^.eMins._z) - fromIntegral (spawnTemp^.stLip)
    modifyRef edictRef (\v -> v & ePos1 .~ pos1
                                & ePos2 .~ pos2
                                & eUse .~ Just usePlat)
    platSpawnInsideTrigger edictRef -- the "start moving" trigger
    setMoveInfoState pos2 (edict^.eTargetName)
    updatedEdict <- readRef edictRef
    soundStart <- (gameImport^.giSoundIndex) (Just "plats/pt1_strt.wav")
    soundMiddle <- (gameImport^.giSoundIndex) (Just "plats/pt1_mid.wav")
    soundEnd <- (gameImport^.giSoundIndex) (Just "plats/pt1_end.wav")
    modifyRef edictRef (\v -> v & eMoveInfo.miSpeed .~ (updatedEdict^.eSpeed)
                                & eMoveInfo.miAccel .~ (updatedEdict^.eAccel)
                                & eMoveInfo.miDecel .~ (updatedEdict^.eDecel)
                                & eMoveInfo.miWait .~ (updatedEdict^.eWait)
                                & eMoveInfo.miStartOrigin .~ (updatedEdict^.ePos1)
                                & eMoveInfo.miStartAngles .~ (updatedEdict^.eEntityState.esAngles)
                                & eMoveInfo.miEndOrigin .~ (updatedEdict^.ePos2)
                                & eMoveInfo.miEndAngles .~ (updatedEdict^.eEntityState.esAngles)
                                & eMoveInfo.miSoundStart .~ soundStart
                                & eMoveInfo.miSoundMiddle .~ soundMiddle
                                & eMoveInfo.miSoundEnd .~ soundEnd)
  where
    checkLip = do
        spawnTemp <- use (gameBaseGlobals.gbSpawnTemp)
        when ((spawnTemp^.stLip) == 0) $
            gameBaseGlobals.gbSpawnTemp.stLip .= 8
    setMoveInfoState pos2 Nothing = do
        modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ pos2)
        linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
        linkEntity edictRef
        modifyRef edictRef (\v -> v & eMoveInfo.miState .~ stateBottom)
    setMoveInfoState _ (Just _) = do
        modifyRef edictRef (\v -> v & eMoveInfo.miState .~ stateUp)


spFuncRotating :: EntThink
spFuncRotating = EntThink "sp_func_rotating" $ \edictRef -> do
    gameImport <- use (gameBaseGlobals.gbGameImport)
    modifyRef edictRef (\v -> v & eSolid .~ Constants.solidBsp)
    updateMoveType edictRef
    -- set the axis of rotation
    updateRotationAxis edictRef
    -- check for reverse rotation
    checkRevereseRotation edictRef
    checkSpeedAndDmg edictRef
    modifyRef edictRef (\v -> v & eUse .~ Just rotatingUse)
    edict <- readRef edictRef
    when ((edict^.eDmg) /= 0) $
        modifyRef edictRef (\v -> v & eBlocked .~ Just rotatingBlocked)
    when ((edict^.eSpawnFlags) .&. 1 /= 0) $ do
        maybe edictUseError (\useF -> entUse useF edictRef Nothing Nothing) (edict^.eUse)
    when ((edict^.eSpawnFlags) .&. 64 /= 0) $
        modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAll))
    when ((edict^.eSpawnFlags) .&. 128 /= 0) $
        modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAllFast))
    (gameImport^.giSetModel) edictRef (edict^.eiModel)
    (gameImport^.giLinkEntity) edictRef
    return True
  where
    edictUseError = Com.fatalError "GameFunc.spFuncRotating edict^.eUse is Nothing"
    updateMoveType edictRef = do
        edict <- readRef edictRef
        let spawnFlags = edict^.eSpawnFlags
            moveType | spawnFlags .&. 32 /= 0 = Constants.moveTypeStop
                     | otherwise              = Constants.moveTypePush
        modifyRef edictRef (\v -> v & eMoveType .~ moveType)
    updateRotationAxis edictRef = do
        edict <- readRef edictRef
        let spawnFlags = edict^.eSpawnFlags
            moveDir | spawnFlags .&. 4 /= 0 = V3 0 0 1
                    | spawnFlags .&. 8 /= 0 = V3 1 0 0
                    | otherwise             = V3 0 1 0
        modifyRef edictRef (\v -> v & eMoveDir .~ moveDir)
    checkRevereseRotation edictRef = do
        edict <- readRef edictRef
        when ((edict^.eSpawnFlags) .&. 2 /= 0) $
            modifyRef edictRef (\v -> v & eMoveDir %~ (fmap (0 -)))
    checkSpeedAndDmg edictRef = do
        edict <- readRef edictRef
        when ((edict^.eSpeed) == 0) $
            modifyRef edictRef (\v -> v & eSpeed .~ 100)
        when ((edict^.eDmg) == 0) $
            modifyRef edictRef (\v -> v & eDmg .~ 2)

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
spFuncTrain edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
{-
    let setModel = gameImport^.giSetModel
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity
        dprintf = gameImport^.giDprintf
-}
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                & eEntityState.esAngles .~ V3 0 0 0
                                & eBlocked .~ Just trainBlocked
                                & eSolid .~ Constants.solidBsp)
    edict <- readRef edictRef
    setDamage edict
    (gameImport^.giSetModel) edictRef (edict^.eiModel)
    noise <- use (gameBaseGlobals.gbSpawnTemp.stNoise)
    when (isJust noise) $ do
        noiseIdx <- (gameImport^.giSoundIndex) noise
        modifyRef edictRef (\v -> v & eMoveInfo.miSoundMiddle .~ noiseIdx)
    let speed = if (edict^.eSpeed) == 0 then 100 else edict^.eSpeed
    modifyRef edictRef (\v -> v & eSpeed .~ speed
                                & eMoveInfo.miSpeed .~ speed
                                & eMoveInfo.miAccel .~ speed
                                & eMoveInfo.miDecel .~ speed
                                & eUse .~ Just trainUse)
    (gameImport^.giLinkEntity) edictRef
    setTarget =<< readRef edictRef
  where
    setDamage edict
        | (edict^.eSpawnFlags) .&. trainBlockStops /= 0 =
            modifyRef edictRef (\v -> v & eDmg .~ 0)
        | otherwise =
            when ((edict^.eDmg) == 0) $
                modifyRef edictRef (\v -> v & eDmg .~ 100)
    setTarget edict
        | isJust (edict^.eTarget) = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            -- start trains on the second frame, to make sure their targets
            -- have had a chance to spawn
            modifyRef edictRef (\v -> v & eThink .~ Just funcTrainFind
                                        & eNextThink .~ levelTime + Constants.frameTime)
        | otherwise = do
            dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
            dprintf (B.concat ["func_train without a target at ", Lib.vtos (edict^.eAbsMin), "\n"])

trainBlocked :: EntBlocked
trainBlocked = error "GameFunc.trainBlocked" -- TODO

spFuncWater :: Ref EdictT -> Quake ()
spFuncWater = error "GameFunc.spFuncWater" -- TODO

spTriggerElevator :: EntThink
spTriggerElevator = error "GameFunc.spTriggerElevator" -- TODO

moveCalc :: Ref EdictT -> V3 Float -> EntThink -> Quake ()
moveCalc edictRef dest func = do
    modifyRef edictRef (\v -> v & eVelocity .~ V3 0 0 0)
    edict <- readRef edictRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    let dir = dest - (edict^.eEntityState.esOrigin)
    modifyRef edictRef (\v -> v & eMoveInfo.miDir .~ normalize dir
                                & eMoveInfo.miRemainingDistance .~ norm dir
                                & eMoveInfo.miEndFunc .~ Just func)
    doMoveCalc edict levelTime
  where
    doMoveCalc edict levelTime
        | (edict^.eMoveInfo.miSpeed) == (edict^.eMoveInfo.miAccel) && (edict^.eMoveInfo.miSpeed) == (edict^.eMoveInfo.miDecel) = do
            currentEntity <- use (gameBaseGlobals.gbLevel.llCurrentEntity)
            let comparedEntity = if (edict^.eFlags) .&. Constants.flTeamSlave /= 0 then edict^.eTeamMaster else Just edictRef
            if currentEntity == comparedEntity
                then void (entThink moveBegin edictRef)
                else modifyRef edictRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime
                                                 & eThink .~ Just moveBegin)
        | otherwise =
            modifyRef edictRef (\v -> v & eMoveInfo.miCurrentSpeed .~ 0
                                        & eThink .~ Just thinkAccelMove
                                        & eNextThink .~ levelTime + Constants.frameTime)

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
funcTrainFind = EntThink "func_train_find" $ \selfRef -> do
    self <- readRef selfRef
    doFuncTrainFind selfRef self (self^.eTarget)
    return True
  where
    doFuncTrainFind _ _ Nothing = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf "train_find: no target\n"
    doFuncTrainFind selfRef self (Just targetName) = do
        entRef <- GameBase.pickTarget (Just targetName)
        maybe (targetNotFound targetName) (proceedFuncTrainFind selfRef self) entRef
    targetNotFound targetName = do
        dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
        dprintf (B.concat ["train_find: target ", targetName, " not found\n"])
    proceedFuncTrainFind selfRef self entRef = do
        ent <- readRef entRef
        modifyRef selfRef (\v -> v & eTarget .~ (ent^.eTarget)
                                   & eEntityState.esOrigin .~ ((ent^.eEntityState.esOrigin) - (self^.eMins)))
        linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
        linkEntity selfRef
        -- if not triggered, start immediately
        when (isNothing (self^.eTargetName)) $
            modifyRef selfRef (\v -> v & eSpawnFlags %~ (.|. trainStartOn))
        updatedSelf <- readRef selfRef
        when ((updatedSelf^.eSpawnFlags) .&. trainStartOn /= 0) $ do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef selfRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime
                                       & eThink .~ Just trainNext
                                       & eActivator .~ Just selfRef)

doorBlocked :: EntBlocked
doorBlocked = error "GameFunc.doorBlocked" -- TODO

doorUse :: EntUse
doorUse = EntUse "door_use" $ \selfRef _ activatorRef -> do
    self <- readRef selfRef
    unless ((self^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
        done <- checkIfDone selfRef self activatorRef
        unless done $ do
            triggerPairedDoors activatorRef (Just selfRef)
  where
    checkIfDone selfRef self activatorRef
        | (self^.eSpawnFlags) .&. Constants.doorToggle /= 0 =
            if (self^.eMoveInfo.miState) == Constants.stateUp || (self^.eMoveInfo.miState) == Constants.stateTop
                then do
                    triggerPairedDoors activatorRef (Just selfRef)
                    return True
                else return False
        | otherwise = return False
    triggerPairedDoors _ Nothing = return ()
    triggerPairedDoors activatorRef (Just edictRef) = do
      edict <- readRef edictRef
      modifyRef edictRef (\v -> v & eMessage .~ Nothing
                                  & eTouch .~ Nothing)
      doorGoUp edictRef activatorRef
      triggerPairedDoors activatorRef (edict^.eTeamChain)

doorGoUp :: Ref EdictT -> Maybe (Ref EdictT) -> Quake ()
doorGoUp selfRef activatorRef = do
    self <- readRef selfRef
    unless ((self^.eMoveInfo.miState) == Constants.stateUp) $
        checkTopState self
  where
    checkTopState self
        | (self^.eMoveInfo.miState) == Constants.stateTop = do
            when ((self^.eMoveInfo.miWait) >= 0) $ do
                levelTime <- use (gameBaseGlobals.gbLevel.llTime)
                modifyRef selfRef (\v -> v & eNextThink .~ levelTime + (self^.eMoveInfo.miWait))
        | otherwise = do
            when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
                when ((self^.eMoveInfo.miSoundStart) /= 0) $ do
                    sound <- use (gameBaseGlobals.gbGameImport.giSound)
                    sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0
                modifyRef selfRef (\v -> v & eEntityState.esSound .~ self^.eMoveInfo.miSoundMiddle)
            modifyRef selfRef (\v -> v & eMoveInfo.miState .~ Constants.stateUp)
            doMoveCalc selfRef self
            GameUtil.useTargets selfRef activatorRef
            doorUseAreaPortals selfRef True
    doMoveCalc selfRef self
        | (self^.eClassName) == "func_door" = moveCalc selfRef (self^.eMoveInfo.miEndOrigin) doorHitTop
        | (self^.eClassName) == "func_door_rotating" = angleMoveCalc selfRef doorHitTop
        | otherwise = return ()

doorHitTop :: EntThink
doorHitTop = EntThink "door_hit_top" $ \selfRef -> do
    self <- readRef selfRef
    when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
        when ((self^.eMoveInfo.miSoundEnd) /= 0) $ do
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0
        modifyRef selfRef (\v -> v & eEntityState.esSound .~ 0)
    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ Constants.stateTop)
    when ((self^.eSpawnFlags) .&. Constants.doorToggle == 0 && (self^.eMoveInfo.miWait) >= 0) $ do
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        modifyRef selfRef (\v -> v & eThink .~ Just doorGoDown
                                   & eNextThink .~ levelTime + (self^.eMoveInfo.miWait))
    return True

doorGoDown :: EntThink
doorGoDown = EntThink "door_go_down" $ \selfRef -> do
    self <- readRef selfRef
    when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
        when ((self^.eMoveInfo.miSoundStart) /= 0) $ do
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0
        modifyRef selfRef (\v -> v & eEntityState.esSound .~ self^.eMoveInfo.miSoundMiddle)
    when ((self^.eMaxHealth) /= 0) $ do
        modifyRef selfRef (\v -> v & eTakeDamage .~ Constants.damageYes
                                   & eHealth .~ self^.eMaxHealth)
    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ Constants.stateDown)
    doMoveCalc selfRef self
    return True
  where
    doMoveCalc selfRef self
        | (self^.eClassName) == "func_door" = moveCalc selfRef (self^.eMoveInfo.miStartOrigin) doorHitBottom
        | (self^.eClassName) == "func_door_rotating" = angleMoveCalc selfRef doorHitBottom
        | otherwise = return ()

doorHitBottom :: EntThink
doorHitBottom = EntThink "door_hit_bottom" $ \selfRef -> do
    self <- readRef selfRef
    when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
        when ((self^.eMoveInfo.miSoundEnd) /= 0) $ do
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0
        modifyRef selfRef (\v -> v & eEntityState.esSound .~ 0)
    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ Constants.stateBottom)
    doorUseAreaPortals selfRef False
    return True

doorKilled :: EntDie
doorKilled = error "GameFunc.doorKilled" -- TODO

doorTouch :: EntTouch
doorTouch = error "GameFunc.doorTouch" -- TODO

thinkCalcMoveSpeed :: EntThink
thinkCalcMoveSpeed = EntThink "think_calc_movespeed" $ \edictRef -> do
    edict <- readRef edictRef
    unless ((edict^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
        minDist <- findSmallestDistance (edict^.eTeamChain) (abs (edict^.eMoveInfo.miDistance))
        adjustSpeeds (Just edictRef) (minDist / (edict^.eMoveInfo.miSpeed))
    return True
  where
    findSmallestDistance Nothing minDist = return minDist
    findSmallestDistance (Just entRef) minDist = do
        ent <- readRef entRef
        findSmallestDistance (ent^.eTeamChain) (min (abs (ent^.eMoveInfo.miDistance)) minDist)
    adjustSpeeds Nothing _ = return ()
    adjustSpeeds (Just entRef) time = do
        ent <- readRef entRef
        let newspeed = (abs (ent^.eMoveInfo.miSpeed)) / time
            ratio = newspeed / (ent^.eMoveInfo.miSpeed)
            accel = if (ent^.eMoveInfo.miAccel) == (ent^.eMoveInfo.miSpeed)
                        then newspeed
                        else (ent^.eMoveInfo.miAccel) * ratio
            decel = if (ent^.eMoveInfo.miDecel) == (ent^.eMoveInfo.miSpeed)
                        then newspeed
                        else (ent^.eMoveInfo.miDecel) * ratio
        modifyRef entRef (\v -> v & eMoveInfo.miAccel .~ accel
                                  & eMoveInfo.miDecel .~ decel
                                  & eMoveInfo.miSpeed .~ newspeed)
        adjustSpeeds (ent^.eTeamChain) time

thinkSpawnDoorTrigger :: EntThink
thinkSpawnDoorTrigger = EntThink "think_spawn_door_trigger" $ \edictRef -> do
    edict <- readRef edictRef
    unless ((edict^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
        (mins, maxs) <- teamChainAddPointToBound (edict^.eTeamChain) (edict^.eMins) (edict^.eMaxs)
        otherRef <- GameUtil.spawn
        modifyRef otherRef (\v -> v & eMins .~ ((V3 (-60) (-60) 0) + mins)
                                    & eMaxs .~ ((V3 60 60 0) + maxs)
                                    & eOwner .~ Just edictRef
                                    & eSolid .~ Constants.solidTrigger
                                    & eMoveType .~ Constants.moveTypeNone
                                    & eTouch .~ Just touchDoorTrigger)
        linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
        linkEntity otherRef
        when ((edict^.eSpawnFlags) .&. Constants.doorStartOpen /= 0) $
            doorUseAreaPortals edictRef True
        void (entThink thinkCalcMoveSpeed edictRef)
    return True
  where
    teamChainAddPointToBound Nothing mins maxs = return (mins, maxs)
    teamChainAddPointToBound (Just otherRef) mins maxs = do
        other <- readRef otherRef
        let (mins', maxs') = GameBase.addPointToBounds (other^.eAbsMin) mins maxs
            (mins'', maxs'') = GameBase.addPointToBounds (other^.eAbsMax) mins' maxs'
        teamChainAddPointToBound (other^.eTeamChain) mins'' maxs''

touchDoorTrigger :: EntTouch
touchDoorTrigger = EntTouch "touch_door_trigger" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    other <- readRef otherRef
    maybe ownerError (doTouchDoorTrigger selfRef self otherRef other) (self^.eOwner)
  where
    ownerError = Com.fatalError "GameFunc.touchDoorTrigger self^.eOwner is Nothing"
    doTouchDoorTrigger selfRef self otherRef other ownerRef = do
        owner <- readRef ownerRef
        levelTime <- use (gameBaseGlobals.gbLevel.llTime)
        unless (shouldSkip self other owner levelTime) $ do
            modifyRef selfRef (\v -> v & eTouchDebounceTime .~ levelTime + 1)
            entUse doorUse ownerRef (Just otherRef) (Just otherRef)
    shouldSkip self other owner levelTime =
        (other^.eHealth) <= 0 
            || ((other^.eSvFlags) .&. Constants.svfMonster == 0 && isNothing (other^.eClient))
            || ((owner^.eSpawnFlags) .&. Constants.doorNoMonster /= 0 && (other^.eSvFlags) .&. Constants.svfMonster /= 0)
            || levelTime < (self^.eTouchDebounceTime)

doorSecretBlocked :: EntBlocked
doorSecretBlocked = EntBlocked "door_secret_blocked" $ \selfRef otherRef -> do
    self <- readRef selfRef
    other <- readRef otherRef
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    v3o <- use (globals.gVec3Origin)
    doDoorSecretBlocked selfRef self otherRef other levelTime v3o
  where
    doDoorSecretBlocked selfRef self otherRef other levelTime v3o
        | (other^.eSvFlags) .&. Constants.svfMonster == 0 && isNothing (other^.eClient) = do
            -- give it a chance to go away on it's own terms (like gibs)
            GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o 100000 1 0 Constants.modCrush
            -- if it's still there, nuke it
            -- TODO: are we sure it is the correct way? (jake2 has different stuff here)
            other' <- readRef otherRef
            when (other'^.eInUse) $
                GameMisc.becomeExplosion1 otherRef
        | levelTime < (self^.eTouchDebounceTime) = return ()
        | otherwise = do
            modifyRef selfRef (\v -> v & eTouchDebounceTime .~ levelTime + 0.5)
            GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

doorSecretUse :: EntUse
doorSecretUse = EntUse "door_secret_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    v3o <- use (globals.gVec3Origin)
    doDoorSecretUse selfRef self v3o
  where
    doDoorSecretUse selfRef self v3o
        -- make sure we're not already moving
        | not ((self^.eEntityState.esOrigin) == v3o) = return ()
        | otherwise = do
            moveCalc selfRef (self^.ePos1) doorSecretMove1
            doorUseAreaPortals selfRef True

doorSecretDie :: EntDie
doorSecretDie = EntDie "door_secret_die" $ \selfRef _ attackerRef _ _ -> do
    modifyRef selfRef (\v -> v & eTakeDamage .~ Constants.damageNo)
    entUse doorSecretUse selfRef (Just attackerRef) (Just attackerRef)

doorUseAreaPortals :: Ref EdictT -> Bool -> Quake ()
doorUseAreaPortals selfRef open = do
    self <- readRef selfRef
    maybe (return ()) (\target -> setAreaPortals target Nothing) (self^.eTarget)
  where
    setAreaPortals target ref = do
        foundRef <- GameBase.gFind ref GameBase.findByTarget target
        maybe (return ()) (doSetAreaPortals target) foundRef
    doSetAreaPortals target foundRef = do
        foundEdict <- readRef foundRef
        when (BC.map toLower (foundEdict^.eClassName) == "func_areaportal") $ do
            setAreaPortalState <- use (gameBaseGlobals.gbGameImport.giSetAreaPortalState)
            setAreaPortalState (foundEdict^.eStyle) open
        setAreaPortals target (Just foundRef)

doorSecretMove1 :: EntThink
doorSecretMove1 = EntThink "door_secret_move1" $ \selfRef -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eNextThink .~ levelTime + 1.0
                               & eThink .~ Just doorSecretMove2)
    return True

doorSecretMove2 :: EntThink
doorSecretMove2 = EntThink "door_secret_move2" $ \selfRef -> do
    self <- readRef selfRef
    moveCalc selfRef (self^.ePos2) doorSecretMove3
    return True

doorSecretMove3 :: EntThink
doorSecretMove3 = EntThink "door_secret_move3" $ \selfRef -> do
    self <- readRef selfRef
    doSecretMove selfRef self
  where
    doSecretMove selfRef self
        | (self^.eWait) == -1 = return True
        | otherwise = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef selfRef (\v -> v & eNextThink .~ levelTime + (self^.eWait)
                                       & eThink .~ Just doorSecretMove4)
            return True

doorSecretMove4 :: EntThink
doorSecretMove4 = EntThink "door_secret_move4" $ \selfRef -> do
    self <- readRef selfRef
    moveCalc selfRef (self^.ePos1) doorSecretMove5
    return True

doorSecretMove5 :: EntThink
doorSecretMove5 = EntThink "door_secret_move5" $ \selfRef -> do
    levelTime <- use (gameBaseGlobals.gbLevel.llTime)
    modifyRef selfRef (\v -> v & eNextThink .~ levelTime + 1.0
                               & eThink .~ Just doorSecretMove6)
    return True

doorSecretMove6 :: EntThink
doorSecretMove6 = EntThink "door_secret_move6" $ \selfRef -> do
    v3o <- use (globals.gVec3Origin)
    moveCalc selfRef v3o doorSecretDone
    return True

doorSecretDone :: EntThink
doorSecretDone = EntThink "door_secret_move7" $ \selfRef -> do
    self <- readRef selfRef
    when (isNothing (self^.eTargetName) || (self^.eSpawnFlags) .&. secretAlwaysShoot /= 0) $
        modifyRef selfRef (\v -> v & eHealth .~ 0
                                   & eTakeDamage .~ Constants.damageYes)
    doorUseAreaPortals selfRef False
    return True

rotatingUse :: EntUse
rotatingUse = EntUse "rotating_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    v3o <- use (globals.gVec3Origin)
    doRotatingUse selfRef self v3o
  where
    doRotatingUse selfRef self v3o
        | (self^.eAVelocity) /= v3o =
            modifyRef selfRef (\v -> v & eEntityState.esSound .~ 0
                                       & eAVelocity .~ V3 0 0 0
                                       & eTouch .~ Nothing)
        | otherwise = do
            modifyRef selfRef (\v -> v & eEntityState.esSound .~ (self^.eMoveInfo.miSoundMiddle)
                                       & eAVelocity .~ fmap (* (self^.eSpeed)) (self^.eMoveDir))
            when ((self^.eSpawnFlags) .&. 16 /= 0) $
                modifyRef selfRef (\v -> v & eTouch .~ Just rotatingTouch)

rotatingBlocked :: EntBlocked
rotatingBlocked = EntBlocked "rotating_blocked" $ \selfRef otherRef -> do
    self <- readRef selfRef
    other <- readRef otherRef
    v3o <- use (globals.gVec3Origin)
    GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

rotatingTouch :: EntTouch
rotatingTouch = EntTouch "rotating_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    when ((self^.eAVelocity._x) /= 0 || (self^.eAVelocity._y) /= 0 || (self^.eAVelocity._z) /= 0) $ do
        other <- readRef otherRef
        v3o <- use (globals.gVec3Origin)
        GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

usePlat :: EntUse
usePlat = EntUse "use_plat" $ \edictRef _ _ -> do
    edict <- readRef edictRef
    maybe (void (entThink platGoDown edictRef)) (\_ -> return ()) (edict^.eThink)

platSpawnInsideTrigger :: Ref EdictT -> Quake ()
platSpawnInsideTrigger edictRef = do
    edict <- readRef edictRef
    spawnTemp <- use (gameBaseGlobals.gbSpawnTemp)
    triggerRef <- GameUtil.spawn
    let tmin = (edict^.eMins) + V3 25 25 0
        tmax = (edict^.eMaxs) + V3 (-25) (-25) 8
        tmin' = tmin & _z .~ (tmax^._z) - ((edict^.ePos1._z) - (edict^.ePos2._z) + fromIntegral (spawnTemp^.stLip))
        tmax' = if (edict^.eSpawnFlags) .&. platLowTrigger /= 0
                    then tmax & _z .~ (tmin'^._z) + 8
                    else tmax
        (tmin'', tmax'') = if (tmax'^._x) - (tmin'^._x) <= 0
                               then let tmin'' = tmin' & _x .~ ((edict^.eMins._x) + (edict^.eMaxs._x)) * 0.5
                                        tmax'' = tmax' & _x .~ (tmin''^._x) + 1
                                    in (tmin'', tmax'')
                               else (tmin', tmax')
        (tmin''', tmax''') = if (tmax''^._y) - (tmin''^._y) <= 0
                                 then let tmin''' = tmin'' & _y .~ ((edict^.eMins._y) + (edict^.eMaxs._y)) * 0.5
                                          tmax''' = tmax'' & _y .~ (tmin''^._y) + 1
                                      in (tmin''', tmax''')
                                 else (tmin'', tmax'')
    modifyRef triggerRef (\v -> v & eTouch .~ Just touchPlatCenter
                                  & eMoveType .~ Constants.moveTypeNone
                                  & eSolid .~ Constants.solidTrigger
                                  & eEnemy .~ Just edictRef
                                  & eMins .~ tmin'''
                                  & eMaxs .~ tmax''')
    linkEntity <- use (gameBaseGlobals.gbGameImport.giLinkEntity)
    linkEntity triggerRef

platBlocked :: EntBlocked
platBlocked = error "GameFunc.platBlocked" -- TODO

platGoDown :: EntThink
platGoDown = EntThink "plat_go_down" $ \edictRef -> do
    edict <- readRef edictRef
    when ((edict^.eFlags) .&. Constants.flTeamSlave == 0) $ do
        when ((edict^.eMoveInfo.miSoundStart) /= 0) $ do
            sound <- use (gameBaseGlobals.gbGameImport.giSound)
            sound (Just edictRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (edict^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0
        modifyRef edictRef (\v -> v & eEntityState.esSound .~ (edict^.eMoveInfo.miSoundMiddle))
    modifyRef edictRef (\v -> v & eMoveInfo.miState .~ stateDown)
    moveCalc edictRef (edict^.eMoveInfo.miEndOrigin) platHitBottom
    return True

platHitBottom :: EntThink
platHitBottom = EntThink "plat_hit_bottom" $ \edictRef -> do
    edict <- readRef edictRef
    when ((edict^.eFlags) .&. Constants.flTeamSlave == 0) $ do
        when ((edict^.eMoveInfo.miSoundEnd) /= 0) $ do
            sound <- use $ gameBaseGlobals.gbGameImport.giSound
            sound (Just edictRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (edict^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0
        modifyRef edictRef (\v -> v & eEntityState.esSound .~ 0)
    modifyRef edictRef (\v -> v & eMoveInfo.miState .~ stateBottom)
    return True

touchPlatCenter :: EntTouch
touchPlatCenter = error "GameFunc.touchPlatCenter" -- TODO

angleMoveCalc :: Ref EdictT -> EntThink -> Quake ()
angleMoveCalc = error "GameFunc.angleMoveCalc" -- TODO

moveBegin :: EntThink
moveBegin = EntThink "move_begin" $ \edictRef -> do
    edict <- readRef edictRef
    doMoveBegin edictRef edict
    return True
  where
    doMoveBegin edictRef edict
        | (edict^.eMoveInfo.miSpeed) * Constants.frameTime >= (edict^.eMoveInfo.miRemainingDistance) =
            void (entThink moveFinal edictRef)
        | otherwise = do
            let velocity = fmap (* (edict^.eMoveInfo.miSpeed)) (edict^.eMoveInfo.miDir)
                frames = floor (((edict^.eMoveInfo.miRemainingDistance) / (edict^.eMoveInfo.miSpeed)) / Constants.frameTime) :: Int
                framesF = fromIntegral frames :: Float
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef edictRef (\v -> v & eVelocity .~ velocity
                                        & eMoveInfo.miRemainingDistance -~ framesF * (edict^.eMoveInfo.miSpeed) * Constants.frameTime
                                        & eNextThink .~ levelTime + (framesF * Constants.frameTime)
                                        & eThink .~ Just moveFinal)

moveFinal :: EntThink
moveFinal = EntThink "move_final" $ \edictRef -> do
    edict <- readRef edictRef
    doMoveFinal edictRef edict
    return True
  where
    doMoveFinal edictRef edict
        | (edict^.eMoveInfo.miRemainingDistance) == 0 =
            void (entThink moveDone edictRef)
        | otherwise = do
            let velocity = fmap (* ((edict^.eMoveInfo.miRemainingDistance) / Constants.frameTime)) (edict^.eMoveInfo.miDir)
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef edictRef (\v -> v & eVelocity .~ velocity
                                        & eThink .~ Just moveDone
                                        & eNextThink .~ levelTime + Constants.frameTime)

moveDone :: EntThink
moveDone = EntThink "move_done" $ \edictRef -> do
    modifyRef edictRef (\v -> v & eVelocity .~ V3 0 0 0)
    edict <- readRef edictRef
    maybe endError (thinkEnd edictRef) (edict^.eMoveInfo.miEndFunc)
    return True
  where
    endError = Com.fatalError "GameFunc.moveDone edict^.eMoveInfo.miEndFunc is Nothing"
    thinkEnd edictRef endF = void (entThink endF edictRef)

thinkAccelMove :: EntThink
thinkAccelMove = EntThink "think_accelmove" $ \edictRef -> do
    edict <- readRef edictRef
    modifyRef edictRef (\v -> v & eMoveInfo.miRemainingDistance -~ (edict^.eMoveInfo.miCurrentSpeed))
    when ((edict^.eMoveInfo.miCurrentSpeed) == 0) $ -- starting or blocked
        platCalcAcceleratedMove edictRef
    platAccelerate edictRef
    -- will the entire move complete on next frame?
    checkMoveComplete edictRef =<< readRef edictRef
    return True
  where
    checkMoveComplete edictRef edict
        | (edict^.eMoveInfo.miRemainingDistance) <= (edict^.eMoveInfo.miCurrentSpeed) =
            void (entThink moveFinal edictRef)
        | otherwise = do
            levelTime <- use (gameBaseGlobals.gbLevel.llTime)
            modifyRef edictRef (\v -> v & eVelocity .~ fmap (* ((edict^.eMoveInfo.miCurrentSpeed) * 10)) (edict^.eMoveInfo.miDir)
                                        & eNextThink .~ levelTime + Constants.frameTime
                                        & eThink .~ Just thinkAccelMove)

platAccelerate :: Ref EdictT -> Quake ()
platAccelerate edictRef = do
    edict <- readRef edictRef
    doPlatAccelerate edictRef (edict^.eMoveInfo)
  where
    doPlatAccelerate edictRef moveInfo
          -- are we decelerating?
        | (moveInfo^.miRemainingDistance) <= (moveInfo^.miDecelDistance) =
            when ((moveInfo^.miRemainingDistance) < (moveInfo^.miDecelDistance)) $
              if (moveInfo^.miNextSpeed) /= 0
                  then modifyRef edictRef (\v -> v & eMoveInfo.miCurrentSpeed .~ (moveInfo^.miNextSpeed)
                                                   & eMoveInfo.miNextSpeed .~ 0)
                  else when ((moveInfo^.miCurrentSpeed) > (moveInfo^.miDecel)) $
                           modifyRef edictRef (\v -> v & eMoveInfo.miCurrentSpeed -~ (moveInfo^.miDecel))
          -- are we at full speed and need to start decelerating during this move?
        | (moveInfo^.miCurrentSpeed) == (moveInfo^.miMoveSpeed) && (moveInfo^.miRemainingDistance) - (moveInfo^.miCurrentSpeed) < (moveInfo^.miDecelDistance) = do
            let p1Distance = (moveInfo^.miRemainingDistance) - (moveInfo^.miDecelDistance)
                p2Distance = (moveInfo^.miMoveSpeed) * (1.0 - (p1Distance / (moveInfo^.miMoveSpeed)))
                distance = p1Distance + p2Distance
            modifyRef edictRef (\v -> v & eMoveInfo.miCurrentSpeed .~ (moveInfo^.miMoveSpeed)
                                        & eMoveInfo.miNextSpeed .~ (moveInfo^.miMoveSpeed) - (moveInfo^.miDecel) * (p2Distance / distance))
          -- are we accelerating?
        | (moveInfo^.miCurrentSpeed) < (moveInfo^.miSpeed) = do
            let oldSpeed = moveInfo^.miCurrentSpeed
                -- figure simple acceleration up to move_speed
                speed = (moveInfo^.miCurrentSpeed) + (moveInfo^.miAccel)
                currentSpeed = if speed > (moveInfo^.miSpeed) then moveInfo^.miSpeed else speed
            modifyRef edictRef (\v -> v & eMoveInfo.miCurrentSpeed .~ currentSpeed)
            -- are we accelerating throughout this entire move?
            unless ((moveInfo^.miRemainingDistance) - currentSpeed >= (moveInfo^.miDecelDistance)) $ do
                -- during this move we will accelerate from current_speed to
                -- move_speed and cross over the decel_distance; figure the
                -- average speed for the entire move
                let p1Distance = (moveInfo^.miRemainingDistance) - (moveInfo^.miDecelDistance)
                    p1Speed = (oldSpeed + (moveInfo^.miMoveSpeed)) / 2.0
                    p2Distance = (moveInfo^.miMoveSpeed) * (1.0 - (p1Distance / p1Speed))
                    distance = p1Distance + p2Distance
                modifyRef edictRef (\v -> v & eMoveInfo.miCurrentSpeed .~ (p1Speed * (p1Distance / distance)) + ((moveInfo^.miMoveSpeed) * (p2Distance / distance))
                                            & eMoveInfo.miNextSpeed .~ (moveInfo^.miMoveSpeed) - (moveInfo^.miDecel) * (p2Distance / distance))
          -- we are at constant velocity (move_speed)
        | otherwise = return ()

platCalcAcceleratedMove :: Ref EdictT -> Quake ()
platCalcAcceleratedMove edictRef = do
    setMoveSpeed
    edict <- readRef edictRef
    doPlatCalcAcceleratedMove (edict^.eMoveInfo)
  where
    doPlatCalcAcceleratedMove moveInfo
        | (moveInfo^.miRemainingDistance) < (moveInfo^.miAccel) =
            modifyRef edictRef (\v -> v & eMoveInfo.miCurrentSpeed .~ (moveInfo^.miRemainingDistance))
        | otherwise = do
            let accelDist = accelerationDistance (moveInfo^.miSpeed) (moveInfo^.miAccel)
                decelDist = accelerationDistance (moveInfo^.miSpeed) (moveInfo^.miDecel)
            if (moveInfo^.miRemainingDistance) - accelDist - decelDist < 0
                then do
                    let f = ((moveInfo^.miAccel) + (moveInfo^.miDecel)) / ((moveInfo^.miAccel) * (moveInfo^.miDecel))
                        moveSpeed = (-2 + sqrt (4 - 4 * f * (-2 * (moveInfo^.miRemainingDistance)))) / (2 * f)
                        decelDist' = accelerationDistance moveSpeed (moveInfo^.miDecel)
                    modifyRef edictRef (\v -> v & eMoveInfo.miMoveSpeed .~ moveSpeed
                                                   & eMoveInfo.miDecelDistance .~ decelDist')
                else do
                    modifyRef edictRef (\v -> v & eMoveInfo.miDecelDistance .~ decelDist)
    accelerationDistance target rate = target * ((target / rate) + 1) / 2
    setMoveSpeed = do
        edict <- readRef edictRef
        modifyRef edictRef (\v -> v & eMoveInfo.miMoveSpeed .~ (edict^.eMoveInfo.miSpeed))

