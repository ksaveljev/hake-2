{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameFunc ( spFuncButton
                     , spFuncDoor
                     , spFuncDoorSecret
                     , spFuncDoorRotating
                     , spFuncRotating
                     , spFuncConveyor
                     , spFuncKillBox
                     , spTriggerElevator
                     , spFuncPlat
                     , spFuncWater
                     , spFuncTrain
                     , spFuncTimer
                     , funcTrainFind
                     , trainUse
                     ) where

import Control.Lens (use, preuse, (.=), (^.), ix, zoom, (%=), (-=), (&), (.~), (%~), (-~))
import Control.Monad (when, liftM, void, unless)
import Data.Bits ((.&.), (.|.), complement)
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, isNothing)
import Linear (V3(..), _x, _y, _z, normalize, quadrance, dot, norm)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Game.EdictT
import Game.GClientT
import Game.MoveInfoT
import Game.ClientPersistantT
import Game.ClientRespawnT
import Game.MonsterInfoT
import Game.PlayerStateT
import Types
import QuakeRef
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import {-# SOURCE #-} qualified Game.GameCombat as GameCombat
import {-# SOURCE #-} qualified Game.GameMisc as GameMisc
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

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
  GenericEntThink "sp_func_button" $ \edictRef -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    GameBase.setMoveDir edictRef

    edict <- readRef edictRef

    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypeStop
                                   & eSolid .~ Constants.solidBsp)

    setModel edictRef (edict^.eiModel)

    when ((edict^.eSounds) /= 1) $ do
      soundIdx <- soundIndex (Just "switches/butn2.wav")
      modifyRef edictRef (\v -> v & eMoveInfo.miSoundStart .~ soundIdx)

    let speed = if (edict^.eSpeed) == 0 then 40 else edict^.eSpeed
    modifyRef edictRef (\v -> v & eSpeed .~ speed)

    when ((edict^.eAccel) == 0) $
      modifyRef edictRef (\v -> v & eAccel .~ speed)

    when ((edict^.eDecel) == 0) $
      modifyRef edictRef (\v -> v & eDecel .~ speed)

    when ((edict^.eWait) == 0) $
      modifyRef edictRef (\v -> v & eWait .~ 3)
    
    lip <- use $ gameBaseGlobals.gbSpawnTemp.stLip
    when (lip == 0) $
      gameBaseGlobals.gbSpawnTemp.stLip .= 4

    let origin = edict^.eEntityState.esOrigin
    modifyRef edictRef (\v -> v & ePos1 .~ origin)

    lip' <- use $ gameBaseGlobals.gbSpawnTemp.stLip
    let moveDir = edict^.eMoveDir
        absMoveDir = fmap abs moveDir
        size = edict^.eSize
        dist = (absMoveDir^._x) * (size^._x)
             + (absMoveDir^._y) * (size^._y)
             + (absMoveDir^._z) * (size^._z)
             - (fromIntegral lip')

    let pos2 = origin + fmap (* dist) moveDir
    modifyRef edictRef (\v -> v & ePos2 .~ pos2
                                   & eUse .~ Just buttonUse
                                   & eEntityState.esEffects %~ (.|. Constants.efAnim01))

    if (edict^.eHealth) /= 0
      then
        modifyRef edictRef (\v -> v & eMaxHealth .~ (edict^.eHealth)
                                       & eDie .~ Just buttonKilled
                                       & eTakeDamage .~ Constants.damageYes)

      else
        when (isNothing (edict^.eTargetName)) $
          modifyRef edictRef (\v -> v & eTouch .~ Just buttonTouch)

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

    linkEntity edictRef
    return True

spFuncDoor :: EntThink
spFuncDoor =
  GenericEntThink "sp_func_door" $ \edictRef -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    GameBase.setMoveDir edictRef

    edict <- readRef edictRef

    when ((edict^.eSounds) /= 1) $ do
      soundStart <- soundIndex (Just "doors/dr1_strt.wav")
      soundMiddle <- soundIndex (Just "doors/dr1_mid.wav")
      soundEnd <- soundIndex (Just "doors/dr1_end.wav")

      modifyRef edictRef (\v -> v & eMoveInfo.miSoundStart .~ soundStart
                                     & eMoveInfo.miSoundMiddle .~ soundMiddle
                                     & eMoveInfo.miSoundEnd .~ soundEnd)

    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                   & eSolid .~ Constants.solidBsp)

    setModel edictRef (edict^.eiModel)

    modifyRef edictRef (\v -> v & eBlocked .~ Just doorBlocked
                                   & eUse .~ Just doorUse)

    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    let speed = if (edict^.eSpeed) == 0 then 100 else edict^.eSpeed
        speed' = if deathmatchValue /= 0 then speed * 2 else speed

    modifyRef edictRef (\v -> v & eSpeed .~ speed')

    when ((edict^.eAccel) == 0) $
      modifyRef edictRef (\v -> v & eAccel .~ speed')

    when ((edict^.eDecel) == 0) $
      modifyRef edictRef (\v -> v & eDecel .~ speed')

    when ((edict^.eWait) == 0) $
      modifyRef edictRef (\v -> v & eWait .~ 3)

    when ((edict^.eDmg) == 0) $
      modifyRef edictRef (\v -> v & eDmg .~ 2)
    
    lip <- use $ gameBaseGlobals.gbSpawnTemp.stLip
    when (lip == 0) $
      gameBaseGlobals.gbSpawnTemp.stLip .= 8

    -- calculate second position
    let origin = edict^.eEntityState.esOrigin
    modifyRef edictRef (\v -> v & ePos1 .~ origin)

    lip' <- use $ gameBaseGlobals.gbSpawnTemp.stLip
    let moveDir = edict^.eMoveDir
        absMoveDir = fmap abs moveDir
        size = edict^.eSize
        dist = (absMoveDir^._x) * (size^._x)
             + (absMoveDir^._y) * (size^._y)
             + (absMoveDir^._z) * (size^._z)
             - (fromIntegral lip')

    modifyRef edictRef (\v -> v & eMoveInfo.miDistance .~ dist)

    let pos2 = origin + fmap (* dist) moveDir
    modifyRef edictRef (\v -> v & ePos2 .~ pos2)

    -- if it starts open, switch the positions
    when ((edict^.eSpawnFlags) .&. Constants.doorStartOpen /= 0) $ do
      modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ pos2
                                     & ePos2 .~ origin
                                     & ePos1 .~ pos2)

    modifyRef edictRef (\v -> v & eMoveInfo.miState .~ Constants.stateBottom)

    if (edict^.eHealth) /= 0
      then
        modifyRef edictRef (\v -> v & eTakeDamage .~ Constants.damageYes
                                       & eMaxHealth .~ (edict^.eHealth)
                                       & eDie .~ Just doorKilled)

      else
        when (isJust (edict^.eTargetName) && isJust (edict^.eMessage)) $ do
          void $ soundIndex (Just "misc/talk.wav")
          modifyRef edictRef (\v -> v & eTouch .~ Just doorTouch)

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

    linkEntity edictRef

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyRef edictRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime)

    let nextThink = if (updatedEdict^.eHealth) /= 0 || isJust (updatedEdict^.eTargetName)
                      then thinkCalcMoveSpeed
                      else thinkSpawnDoorTrigger

    modifyRef edictRef (\v -> v & eThink .~ Just nextThink)

    return True

spFuncDoorSecret :: EntThink
spFuncDoorSecret =
  GenericEntThink "sp_func_door_secret" $ \edictRef -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let soundIndex = gameImport^.giSoundIndex
        setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    soundStart <- soundIndex (Just "doors/dr1_strt.wav")
    soundMiddle <- soundIndex (Just "doors/dr1_mid.wav")
    soundEnd <- soundIndex (Just "doors/dr1_end.wav")

    modifyRef edictRef (\v -> v & eMoveInfo.miSoundStart .~ soundStart
                                   & eMoveInfo.miSoundMiddle .~ soundMiddle
                                   & eMoveInfo.miSoundEnd .~ soundEnd
                                   & eMoveType .~ Constants.moveTypePush
                                   & eSolid .~ Constants.solidBsp
                                   & eBlocked .~ Just doorSecretBlocked
                                   & eUse .~ Just doorSecretUse)

    edict <- readRef edictRef

    setModel edictRef (edict^.eiModel)

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

    -- calculate positions
    let (Just forward, Just right, Just up) = Math3D.angleVectors (edict^.eEntityState.esAngles) True True True

    modifyRef edictRef (\v -> v & eEntityState.esAngles .~ V3 0 0 0)

    let side = 1.0 - fromIntegral ((edict^.eSpawnFlags) .&. secretFirstLeft)
        width = if (edict^.eSpawnFlags) .&. secretFirstDown /= 0
                  then abs (up `dot` (edict^.eSize))
                  else abs (right `dot` (edict^.eSize))
        length = abs (forward `dot` (edict^.eSize))
        pos1 = if (edict^.eSpawnFlags) .&. secretFirstDown /= 0
                 then (edict^.eEntityState.esOrigin) + fmap (* ((-1) * width)) up
                 else (edict^.eEntityState.esOrigin) + fmap (* (side * width)) right
        pos2 = pos1 + fmap (* length) forward

    modifyRef edictRef (\v -> v & ePos1 .~ pos1
                                   & ePos2 .~ pos2)

    if (edict^.eHealth) /= 0
      then
        modifyRef edictRef (\v -> v & eTakeDamage .~ Constants.damageYes
                                       & eDie .~ Just doorKilled
                                       & eMaxHealth .~ (edict^.eHealth))
      else do
        void $ soundIndex (Just "misc/talk.wav")
        modifyRef edictRef (\v -> v & eTouch .~ Just doorTouch)

    modifyRef edictRef (\v -> v & eClassName .~ "func_door")

    linkEntity edictRef
    return True

doorSecretBlocked :: EntBlocked
doorSecretBlocked =
  GenericEntBlocked "door_secret_blocked" $ \selfRef otherRef -> do
    self <- readRef selfRef
    other <- readRef otherRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    v3o <- use $ globals.gVec3Origin

    if | (other^.eSvFlags) .&. Constants.svfMonster == 0 && isNothing (other^.eClient) -> do
           -- give it a chance to go away on it's own terms (like gibs)
           GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o 100000 1 0 Constants.modCrush
           -- if it's still there, nuke it
           -- RESEARCH: are we sure it is the correct way? (jake2 has different stuff here)
           other' <- readRef otherRef
           when (other'^.eInUse) $
             GameMisc.becomeExplosion1 otherRef

       | levelTime < (self^.eTouchDebounceTime) ->
           return ()

       | otherwise -> do
           modifyRef selfRef (\v -> v & eTouchDebounceTime .~ levelTime + 0.5)
           GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

doorSecretUse :: EntUse
doorSecretUse =
  GenericEntUse "door_secret_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    v3o <- use $ globals.gVec3Origin

    -- make sure we're not already moving
    if not ((self^.eEntityState.esOrigin) == v3o)
      then
        return ()
      else do
        moveCalc selfRef (self^.ePos1) doorSecretMove1
        doorUseAreaPortals selfRef True

doorSecretMove1 :: EntThink
doorSecretMove1 =
  GenericEntThink "door_secret_move1" $ \selfRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyRef selfRef (\v -> v & eNextThink .~ levelTime + 1.0
                                  & eThink .~ Just doorSecretMove2)
    return True

doorSecretMove2 :: EntThink
doorSecretMove2 =
  GenericEntThink "door_secret_move2" $ \selfRef -> do
    self <- readRef selfRef
    moveCalc selfRef (self^.ePos2) doorSecretMove3
    return True

doorSecretMove3 :: EntThink
doorSecretMove3 =
  GenericEntThink "door_secret_move3" $ \selfRef -> do
    self <- readRef selfRef

    if (self^.eWait) == -1
      then
        return True
      else do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        modifyRef selfRef (\v -> v & eNextThink .~ levelTime + (self^.eWait)
                                      & eThink .~ Just doorSecretMove4)
        return True

doorSecretMove4 :: EntThink
doorSecretMove4 =
  GenericEntThink "door_secret_move4" $ \selfRef -> do
    self <- readRef selfRef
    moveCalc selfRef (self^.ePos1) doorSecretMove5
    return True

doorSecretMove5 :: EntThink
doorSecretMove5 =
  GenericEntThink "door_secret_move5" $ \selfRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyRef selfRef (\v -> v & eNextThink .~ levelTime + 1.0
                                  & eThink .~ Just doorSecretMove6)
    return True

doorSecretMove6 :: EntThink
doorSecretMove6 =
  GenericEntThink "door_secret_move6" $ \selfRef -> do
    v3o <- use $ globals.gVec3Origin
    moveCalc selfRef v3o doorSecretDone
    return True

doorSecretDone :: EntThink
doorSecretDone =
  GenericEntThink "door_secret_move7" $ \selfRef -> do
    self <- readRef selfRef

    when (isNothing (self^.eTargetName) || (self^.eSpawnFlags) .&. secretAlwaysShoot /= 0) $
      modifyRef selfRef (\v -> v & eHealth .~ 0
                                    & eTakeDamage .~ Constants.damageYes)

    doorUseAreaPortals selfRef False
    return True

doorSecretDie :: EntDie
doorSecretDie =
  GenericEntDie "door_secret_die" $ \selfRef _ attackerRef _ _ -> do
    modifyRef selfRef (\v -> v & eTakeDamage .~ Constants.damageNo)
    entUse doorSecretUse selfRef (Just attackerRef) (Just attackerRef)

spFuncDoorRotating :: EntThink
spFuncDoorRotating =
  GenericEntThink "sp_func_door_rotating" $ \edictRef -> do
    edict <- readRef edictRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let dprintf = gameImport^.giDprintf
        setModel = gameImport^.giSetModel
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity

    -- set the axis of rotation
    let moveDir = if | (edict^.eSpawnFlags) .&. doorXAxis /= 0 -> V3 0 0 1.0
                     | (edict^.eSpawnFlags) .&. doorYAxis /= 0 -> V3 1.0 0 0
                     | otherwise -> V3 0 1.0 0
        -- check for reverse rotation
        moveDir' = if (edict^.eSpawnFlags) .&. doorReverse /= 0
                     then fmap negate moveDir
                     else moveDir

    modifyRef edictRef (\v -> v & eEntityState.esAngles .~ V3 0 0 0
                                   & eMoveDir .~ moveDir')
      
    use (gameBaseGlobals.gbSpawnTemp) >>= \spawnTemp ->
      when ((spawnTemp^.stDistance) == 0) $ do
        dprintf ((edict^.eClassName) `B.append` " at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` " with no distance set\n")
        gameBaseGlobals.gbSpawnTemp.stDistance .= 90

    spawnTemp <- use $ gameBaseGlobals.gbSpawnTemp

    let speed = if (edict^.eSpeed) == 0 then 100 else edict^.eSpeed

    modifyRef edictRef (\v -> v & ePos1 .~ V3 0 0 0
                                   & ePos2 .~ fmap (* fromIntegral (spawnTemp^.stDistance)) moveDir'
                                   & eMoveInfo.miDistance .~ fromIntegral (spawnTemp^.stDistance)
                                   & eMoveType .~ Constants.moveTypePush
                                   & eSolid .~ Constants.solidBsp
                                   & eBlocked .~ Just doorBlocked
                                   & eUse .~ Just doorUse
                                   & eSpeed .~ speed
                                   & eAccel %~ (\a -> if a == 0 then speed else a)
                                   & eDecel %~ (\d -> if d == 0 then speed else d)
                                   & eWait %~ (\w -> if w == 0 then 3 else w)
                                   & eDmg %~ (\d -> if d == 0 then 2 else d))

    setModel edictRef (edict^.eiModel)

    when ((edict^.eSounds) /= 1) $ do
      soundStart <- soundIndex (Just "doors/dr1_strt.wav")
      soundMiddle <- soundIndex (Just "doors/dr1_mid.wav")
      soundEnd <- soundIndex (Just "doors/dr1_end.wav")
      modifyRef edictRef (\v -> v & eMoveInfo.miSoundStart .~ soundStart
                                     & eMoveInfo.miSoundMiddle .~ soundMiddle
                                     & eMoveInfo.miSoundEnd .~ soundEnd)

    -- if it starts open, switch the positions
    when ((edict^.eSpawnFlags) .&. Constants.doorStartOpen /= 0) $ do
      edict' <- readRef edictRef
      modifyRef edictRef (\v -> v & eEntityState.esAngles .~ (edict'^.ePos2)
                                     & ePos2 .~ (edict'^.ePos1)
                                     & ePos1 .~ (edict'^.ePos2)
                                     & eMoveDir %~ (fmap negate))

    when ((edict^.eHealth) /= 0) $
      modifyRef edictRef (\v -> v & eTakeDamage .~ Constants.damageYes
                                     & eDie .~ Just doorKilled
                                     & eMaxHealth .~ (edict^.eHealth))

    when (isJust (edict^.eTargetName) && isJust (edict^.eMessage)) $ do
      void $ soundIndex (Just "misc/talk.wav")
      modifyRef edictRef (\v -> v & eTouch .~ Just doorTouch)

    edict' <- readRef edictRef

    modifyRef edictRef (\v -> v & eMoveInfo.miState .~ stateBottom
                                   & eMoveInfo.miSpeed .~ (edict'^.eSpeed)
                                   & eMoveInfo.miAccel .~ (edict'^.eAccel)
                                   & eMoveInfo.miDecel .~ (edict'^.eDecel)
                                   & eMoveInfo.miWait .~ (edict'^.eWait)
                                   & eMoveInfo.miStartOrigin .~ (edict'^.eEntityState.esOrigin)
                                   & eMoveInfo.miStartAngles .~ (edict'^.ePos1)
                                   & eMoveInfo.miEndOrigin .~ (edict'^.eEntityState.esOrigin)
                                   & eMoveInfo.miEndAngles .~ (edict'^.ePos2))

    when ((edict'^.eSpawnFlags) .&. 16 /= 0) $
      modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAll))
      
    -- to simplify logic elsewhere, make non-teamed doors into a team of one
    when (isNothing (edict'^.eTeam)) $
      modifyRef edictRef (\v -> v & eTeamMaster .~ Just edictRef)

    linkEntity edictRef

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyRef edictRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime)

    let nextThink = if (edict'^.eHealth) /= 0 || isJust (edict'^.eTargetName)
                      then Just thinkCalcMoveSpeed
                      else Just thinkSpawnDoorTrigger

    modifyRef edictRef (\v -> v & eThink .~ nextThink)
    return True

spFuncConveyor :: EntThink
spFuncConveyor =
  GenericEntThink "sp_func_conveyor" $ \selfRef -> do
    self <- readRef selfRef

    let speed = if (self^.eSpeed) == 0 then 100 else self^.eSpeed
    modifyRef selfRef (\v -> v & eSpeed .~ speed)

    when ((self^.eSpawnFlags) .&. 1 == 0) $ do
      modifyRef selfRef (\v -> v & eCount .~ truncate speed
                                     & eSpeed .~ 0)

    modifyRef selfRef (\v -> v & eUse .~ Just funcConveyorUse)
    
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    setModel selfRef (self^.eiModel)
    modifyRef selfRef (\v -> v & eSolid .~ Constants.solidBsp)
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
  GenericEntThink "sp_func_killbox" $ \edictRef -> do
    edict <- readRef edictRef
    setModel <- use $ gameBaseGlobals.gbGameImport.giSetModel

    setModel edictRef (edict^.eiModel)

    modifyRef edictRef (\v -> v & eUse .~ Just useKillBox
                                   & eSvFlags .~ Constants.svfNoClient)

    return True

spFuncRotating :: EntThink
spFuncRotating =
  GenericEntThink "sp_func_rotating" $ \edictRef -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

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

    when ((edict^.eSpawnFlags) .&. 1 /= 0) $
      entUse (fromJust $ edict^.eUse) edictRef Nothing Nothing

    when ((edict^.eSpawnFlags) .&. 64 /= 0) $
      modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAll))

    when ((edict^.eSpawnFlags) .&. 128 /= 0) $
      modifyRef edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAllFast))

    setModel edictRef (edict^.eiModel)
    linkEntity edictRef

    return True

  where updateMoveType :: Ref EdictT -> Quake ()
        updateMoveType edictRef = do
          edict <- readRef edictRef
          let spawnFlags = edict^.eSpawnFlags

          let moveType = if spawnFlags .&. 32 /= 0
                           then Constants.moveTypeStop
                           else Constants.moveTypePush

          modifyRef edictRef (\v -> v & eMoveType .~ moveType)

        updateRotationAxis :: Ref EdictT -> Quake ()
        updateRotationAxis edictRef = do
          edict <- readRef edictRef
          let spawnFlags = edict^.eSpawnFlags

          let moveDir = if | spawnFlags .&. 4 /= 0 -> V3 0 0 1
                           | spawnFlags .&. 8 /= 0 -> V3 1 0 0
                           | otherwise -> V3 0 1 0

          modifyRef edictRef (\v -> v & eMoveDir .~ moveDir)

        checkRevereseRotation :: Ref EdictT -> Quake ()
        checkRevereseRotation edictRef = do
          edict <- readRef edictRef
          let spawnFlags = edict^.eSpawnFlags

          when (spawnFlags .&. 2 /= 0) $
            modifyRef edictRef (\v -> v & eMoveDir %~ (fmap (0 -)))

        checkSpeedAndDmg :: Ref EdictT -> Quake ()
        checkSpeedAndDmg edictRef = do
          edict <- readRef edictRef

          when ((edict^.eSpeed) == 0) $
            modifyRef edictRef (\v -> v & eSpeed .~ 100)

          when ((edict^.eDmg) == 0) $
            modifyRef edictRef (\v -> v & eDmg .~ 2)

triggerElevatorInit :: EntThink
triggerElevatorInit =
  GenericEntThink "trigger_elevator_init" $ \selfRef -> do
    self <- readRef selfRef
    dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf

    case self^.eTarget of
      Nothing -> do
        dprintf "trigger_elevator has no target\n"
        return True

      Just target -> do
        maybeMoveTargetRef <- GameBase.pickTarget (self^.eTarget)
        modifyRef selfRef (\v -> v & eMoveTarget .~ maybeMoveTargetRef)

        case maybeMoveTargetRef of
          Nothing -> do
            dprintf ("trigger_elevator unable to find target " `B.append` target `B.append` "\n")
            return True
            
          Just moveTargetRef -> do
            moveTarget <- readRef moveTargetRef

            if (moveTarget^.eClassName) /= "func_train"
              then do
                dprintf ("trigger_elevator target " `B.append` target `B.append` " is not a train\n")
                return True
              else do
                modifyRef selfRef (\v -> v & eUse .~ Just triggerElevatorUse
                                              & eSvFlags .~ Constants.svfNoClient)

                return True

spTriggerElevator :: EntThink
spTriggerElevator =
  GenericEntThink "sp_trigger_elevator" $ \selfRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyRef selfRef (\v -> v & eThink .~ Just triggerElevatorInit
                                  & eNextThink .~ levelTime + Constants.frameTime)

    return True

{-
- QUAKED func_plat (0 .5 .8) ? PLAT_LOW_TRIGGER speed default 150
- 
- Plats are always drawn in the extended position, so they will light
- correctly.
- 
- If the plat is the target of another trigger or button, it will start out
- disabled in the extended position until it is trigger, when it will lower
- and become a normal plat.
- 
- "speed" overrides default 200. "accel" overrides default 500 "lip"
- overrides default 8 pixel lip
- 
- If the "height" key is set, that will determine the amount the plat
- moves, instead of being implicitly determoveinfoned by the model's
- height.
- 
- Set "sounds" to one of the following: 1) base fast 2) chain slow
-}
spFuncPlat :: Ref EdictT -> Quake ()
spFuncPlat edictRef = do
    edict <- readRef edictRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity

    modifyRef edictRef (\v -> v & eEntityState.esAngles .~ V3 0 0 0
                                   & eSolid .~ Constants.solidBsp
                                   & eMoveType .~ Constants.moveTypePush
                                   & eBlocked .~ Just platBlocked
                                   & eSpeed %~ (\s -> if s == 0 then 20 else s * 0.1)
                                   & eAccel %~ (\a -> if a == 0 then 5 else a * 0.1)
                                   & eDecel %~ (\d -> if d == 0 then 5 else d * 0.1)
                                   & eDmg %~ (\d -> if d == 0 then 2 else d))

    use (gameBaseGlobals.gbSpawnTemp) >>= \spawnTemp ->
      when ((spawnTemp^.stLip) == 0) $
        gameBaseGlobals.gbSpawnTemp.stLip .= 8

    setModel edictRef (edict^.eiModel)

    spawnTemp <- use $ gameBaseGlobals.gbSpawnTemp

    -- pos1 is the top position pos2 is the bottom
    let pos1 = edict^.eEntityState.esOrigin
        pos2 = if (spawnTemp^.stHeight) /= 0
                 then (edict^.eEntityState.esOrigin) & _z -~ fromIntegral (spawnTemp^.stHeight)
                 else (edict^.eEntityState.esOrigin) & _z -~ (edict^.eMaxs._z) - (edict^.eMins._z) - fromIntegral (spawnTemp^.stLip)

    modifyRef edictRef (\v -> v & ePos1 .~ pos1
                                   & ePos2 .~ pos2
                                   & eUse .~ Just usePlat)

    platSpawnInsideTrigger edictRef -- the "start moving" trigger

    case edict^.eTargetName of
      Just _ ->
        modifyRef edictRef (\v -> v & eMoveInfo.miState .~ stateUp)

      Nothing -> do
        modifyRef edictRef (\v -> v & eEntityState.esOrigin .~ pos2)
        linkEntity edictRef
        modifyRef edictRef (\v -> v & eMoveInfo.miState .~ stateBottom)

    edict' <- readRef edictRef

    soundStart <- soundIndex (Just "plats/pt1_strt.wav")
    soundMiddle <- soundIndex (Just "plats/pt1_mid.wav")
    soundEnd <- soundIndex (Just "plats/pt1_end.wav")

    modifyRef edictRef (\v -> v & eMoveInfo.miSpeed .~ (edict'^.eSpeed)
                                   & eMoveInfo.miAccel .~ (edict'^.eAccel)
                                   & eMoveInfo.miDecel .~ (edict'^.eDecel)
                                   & eMoveInfo.miWait .~ (edict'^.eWait)
                                   & eMoveInfo.miStartOrigin .~ (edict'^.ePos1)
                                   & eMoveInfo.miStartAngles .~ (edict'^.eEntityState.esAngles)
                                   & eMoveInfo.miEndOrigin .~ (edict'^.ePos2)
                                   & eMoveInfo.miEndAngles .~ (edict'^.eEntityState.esAngles)
                                   & eMoveInfo.miSoundStart .~ soundStart
                                   & eMoveInfo.miSoundMiddle .~ soundMiddle
                                   & eMoveInfo.miSoundEnd .~ soundEnd)

platBlocked :: EntBlocked
platBlocked =
  GenericEntBlocked "plat_blocked" $ \selfRef otherRef -> do
    self <- readRef selfRef
    other <- readRef otherRef
    v3o <- use $ globals.gVec3Origin

    if (other^.eSvFlags) .&. Constants.svfMonster == 0 && isNothing (other^.eClient)
      then do
        -- give it a chance to go away on it's own terms (like gibs)
        GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o 100000 1 0 Constants.modCrush

        -- if it's still there, nuke it
        -- RESEARCH: are we sure it is the correct way? (jake2 has different stuff here)
        other' <- readRef otherRef
        when (other'^.eInUse) $
          GameMisc.becomeExplosion1 otherRef

      else do
        GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

        self' <- readRef selfRef
        if (self'^.eMoveInfo.miState) == stateUp
          then void $ think platGoDown selfRef
          else platGoUp selfRef

platGoDown :: EntThink
platGoDown =
  GenericEntThink "plat_go_down" $ \edictRef -> do
    edict <- readRef edictRef

    when ((edict^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((edict^.eMoveInfo.miSoundStart) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just edictRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (edict^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0

      modifyRef edictRef (\v -> v & eEntityState.esSound .~ (edict^.eMoveInfo.miSoundMiddle))

    modifyRef edictRef (\v -> v & eMoveInfo.miState .~ stateDown)
    moveCalc edictRef (edict^.eMoveInfo.miEndOrigin) platHitBottom
    return True

platHitBottom :: EntThink
platHitBottom =
  GenericEntThink "plat_hit_bottom" $ \edictRef -> do
    edict <- readRef edictRef

    when ((edict^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((edict^.eMoveInfo.miSoundEnd) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just edictRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (edict^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0

      modifyRef edictRef (\v -> v & eEntityState.esSound .~ 0)

    modifyRef edictRef (\v -> v & eMoveInfo.miState .~ stateBottom)
    return True

platGoUp :: Ref EdictT -> Quake ()
platGoUp edictRef = do
    edict <- readRef edictRef

    when ((edict^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((edict^.eMoveInfo.miSoundStart) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just edictRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (edict^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0

      modifyRef edictRef (\v -> v & eEntityState.esSound .~ (edict^.eMoveInfo.miSoundMiddle))

    modifyRef edictRef (\v -> v & eMoveInfo.miState .~ stateUp)
    moveCalc edictRef (edict^.eMoveInfo.miStartOrigin) platHitTop

platHitTop :: EntThink
platHitTop =
  GenericEntThink "plat_hit_top" $ \edictRef -> do
    edict <- readRef edictRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    when ((edict^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((edict^.eMoveInfo.miSoundEnd) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just edictRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (edict^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0

      modifyRef edictRef (\v -> v & eEntityState.esSound .~ 0)

    modifyRef edictRef (\v -> v & eMoveInfo.miState .~ stateTop
                                   & eThink .~ Just platGoDown
                                   & eNextThink .~ levelTime + 3)

    return True

usePlat :: EntUse
usePlat =
  GenericEntUse "use_plat" $ \edictRef _ _ -> do
    edict <- readRef edictRef

    case edict^.eThink of
      Just _ -> return () -- already down
      Nothing -> void $ think platGoDown edictRef

platSpawnInsideTrigger :: Ref EdictT -> Quake ()
platSpawnInsideTrigger edictRef = do
    edict <- readRef edictRef
    spawnTemp <- use $ gameBaseGlobals.gbSpawnTemp
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

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity triggerRef

touchPlatCenter :: EntTouch
touchPlatCenter =
  GenericEntTouch "touch_plat_center" $ \edictRef otherRef _ _ -> do
    edict <- readRef edictRef
    other <- readRef otherRef

    unless (isNothing (other^.eClient) || (other^.eHealth) <= 0) $ do
      -- now point at the plat, not the trigger
      -- RESEARCH: make sure this is what the jake2 code does
      let Just enemyRef = edict^.eEnemy
      enemy <- readRef enemyRef

      if | (enemy^.eMoveInfo.miState) == stateBottom ->
             platGoUp enemyRef

         | (enemy^.eMoveInfo.miState) == stateTop -> do
             levelTime <- use $ gameBaseGlobals.gbLevel.llTime
             modifyRef enemyRef (\v -> v & eNextThink .~ levelTime + 1) -- the player is still on the plat, so delay going down

         | otherwise ->
             return ()

{-
- QUAKED func_water (0 .5 .8) ? START_OPEN func_water is a moveable water
- brush. It must be targeted to operate. Use a non-water texture at your
- own risk.
- 
- START_OPEN causes the water to move to its destination when spawned and
- operate in reverse.
- 
- "angle" determines the opening direction (up or down only) "speed"
- movement speed (25 default) "wait" wait before returning (-1 default, -1 =
- TOGGLE) "lip" lip remaining at end of move (0 default) "sounds" (yes,
- these need to be changed) 0) no sound 1) water 2) lava
-}
spFuncWater :: Ref EdictT -> Quake ()
spFuncWater selfRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity

    GameBase.setMoveDir selfRef

    self <- readRef selfRef
    spawnTemp <- use $ gameBaseGlobals.gbSpawnTemp
    sounds <- case self^.eSounds of
                1 -> do -- water
                       soundStart <- soundIndex (Just "world/mov_watr.wav")
                       soundEnd <- soundIndex (Just "world/stp_watr.wav")
                       return $ Just (soundStart, soundEnd)

                2 -> do -- laval
                       soundStart <- soundIndex (Just "world/mov_watr.wav")
                       soundEnd <- soundIndex (Just "world/stp_watr.wav")
                       return $ Just (soundStart, soundEnd)

                _ -> return Nothing

    case sounds of
      Nothing -> return ()
      Just (soundStart, soundEnd) ->
        modifyRef selfRef (\v -> v & eMoveInfo.miSoundStart .~ soundStart
                                      & eMoveInfo.miSoundEnd .~ soundEnd)

    let absMoveDir = fmap abs (self^.eMoveDir)
        distance = (absMoveDir^._x) * (self^.eSize._x) + (absMoveDir^._y) * (self^.eSize._y) + (absMoveDir^._z) * (self^.eSize._z) - fromIntegral (spawnTemp^.stLip)

    modifyRef selfRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                  & eSolid .~ Constants.solidBsp
                                  & ePos1 .~ (self^.eEntityState.esOrigin)
                                  & eMoveInfo.miDistance .~ distance
                                  & ePos2 .~ (self^.eEntityState.esOrigin) + fmap (* distance) (self^.eMoveDir))

    setModel selfRef (self^.eiModel)

    -- if it starts open, switch the positions
    when ((self^.eSpawnFlags) .&. doorStartOpen /= 0) $ do
      self' <- readRef selfRef
      modifyRef selfRef (\v -> v & eEntityState.esOrigin .~ (self'^.ePos2)
                                    & ePos2 .~ (self'^.ePos1)
                                    & ePos1 .~ (self'^.ePos2))

    self' <- readRef selfRef

    let speed = if (self'^.eSpeed) == 0 then 25 else self'^.eSpeed
        wait = if (self'^.eWait) == 0 then -1 else self'^.eWait

    modifyRef selfRef (\v -> v & eMoveInfo.miStartOrigin .~ (self'^.ePos1)
                                  & eMoveInfo.miStartAngles .~ (self'^.eEntityState.esAngles)
                                  & eMoveInfo.miEndOrigin .~ (self'^.ePos2)
                                  & eMoveInfo.miEndAngles .~ (self'^.eEntityState.esAngles)
                                  & eSpeed .~ speed
                                  & eMoveInfo.miSpeed .~ speed
                                  & eMoveInfo.miAccel .~ speed
                                  & eMoveInfo.miDecel .~ speed
                                  & eWait .~ wait
                                  & eMoveInfo.miWait .~ wait
                                  & eUse .~ Just doorUse
                                  & eSpawnFlags %~ (\f -> if wait == -1 then f .|. doorToggle else f)
                                  & eClassName .~ "func_door")

    linkEntity selfRef

spFuncTrain :: Ref EdictT -> Quake ()
spFuncTrain edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity
        dprintf = gameImport^.giDprintf

    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                   & eEntityState.esAngles .~ V3 0 0 0
                                   & eBlocked .~ Just trainBlocked
                                   & eSolid .~ Constants.solidBsp)

    edict <- readRef edictRef

    if (edict^.eSpawnFlags) .&. trainBlockStops /= 0
      then
        modifyRef edictRef (\v -> v & eDmg .~ 0)

      else
        when ((edict^.eDmg) == 0) $
          modifyRef edictRef (\v -> v & eDmg .~ 100)

    setModel edictRef (edict^.eiModel)

    noise <- use $ gameBaseGlobals.gbSpawnTemp.stNoise

    when (isJust noise) $ do
      noiseIdx <- soundIndex noise
      modifyRef edictRef (\v -> v & eMoveInfo.miSoundMiddle .~ noiseIdx)

    let speed = if (edict^.eSpeed) == 0 then 100 else edict^.eSpeed
    modifyRef edictRef (\v -> v & eSpeed .~ speed
                                   & eMoveInfo.miSpeed .~ speed
                                   & eMoveInfo.miAccel .~ speed
                                   & eMoveInfo.miDecel .~ speed
                                   & eUse .~ Just trainUse)

    linkEntity edictRef

    if isJust (edict^.eTarget)
      then do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        -- start trains on the second frame, to make sure their targets
        -- have had a chance to spawn
        modifyRef edictRef (\v -> v & eThink .~ Just funcTrainFind
                                       & eNextThink .~ levelTime + Constants.frameTime)

      else
        dprintf $ "func_train without a target at " `B.append` Lib.vtos (edict^.eAbsMin) `B.append` "\n"

spFuncTimer :: Ref EdictT -> Quake ()
spFuncTimer edictRef = do
    edict <- readRef edictRef

    when ((edict^.eWait) == 0) $
      modifyRef edictRef (\v -> v & eWait .~ 1)

    modifyRef edictRef (\v -> v & eUse .~ Just funcTimerUse
                                   & eThink .~ Just funcTimerThink)

    when ((edict^.eRandom) >= (edict^.eWait)) $ do
      modifyRef edictRef (\v -> v & eRandom .~ (edict^.eWait) - Constants.frameTime)
      dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
      dprintf $ "func_timer at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` " has random >= wait\n"
    
    when (((edict^.eSpawnFlags) .&. 1) /= 0) $ do
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      pauseTime <- use $ gameBaseGlobals.gbSpawnTemp.stPauseTime
      cr <- Lib.crandom

      modifyRef edictRef (\v -> v & eNextThink .~ levelTime + 1 + pauseTime + (edict^.eDelay) + (edict^.eWait) + cr * (edict^.eRandom)
                                     & eActivator .~ Just edictRef)

    modifyRef edictRef (\v -> v & eSvFlags .~ Constants.svfNoClient)

funcTimerUse :: EntUse
funcTimerUse =
  GenericEntUse "func_timer_use" $ \selfRef _ activator -> do
    modifyRef selfRef (\v -> v & eActivator .~ activator)

    -- if on, turn it off
    self <- readRef selfRef

    if (self^.eNextThink) /= 0
      then
        modifyRef selfRef (\v -> v & eNextThink .~ 0)
      else
        -- turn it on
        if (self^.eDelay) /= 0
          then do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime
            modifyRef selfRef (\v -> v & eNextThink .~ levelTime + (self^.eDelay))
          else
            void $ think funcTimerThink selfRef

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
  GenericEntThink "func_timer_think" $ \edictRef -> do
    edict <- readRef edictRef

    GameUtil.useTargets edictRef (edict^.eActivator)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    r <- Lib.crandom
    modifyRef edictRef (\v -> v & eNextThink .~ levelTime + (edict^.eWait) + r * (edict^.eRandom))

    return True

trainBlocked :: EntBlocked
trainBlocked =
  GenericEntBlocked "train_blocked" $ \selfRef otherRef -> do
    self <- readRef selfRef
    other <- readRef otherRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    v3o <- use $ globals.gVec3Origin

    if | (other^.eSvFlags) .&. Constants.svfMonster == 0 && isNothing (other^.eClient) -> do
           -- give it a chance to go away on it's own terms (like gibs)
           GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o 100000 1 0 Constants.modCrush

           -- if it's still there, nuke it
           -- RESEARCH: are we sure it is the correct way? (jake2 has different stuff here)
           other' <- readRef otherRef
           when (other'^.eInUse) $
             GameMisc.becomeExplosion1 otherRef

       | levelTime < (self^.eTouchDebounceTime) || (self^.eDmg) == 0 ->
           return ()

       | otherwise -> do
           modifyRef selfRef (\v -> v & eTouchDebounceTime .~ levelTime + 0.5)
           GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

trainUse :: EntUse
trainUse =
  GenericEntUse "train_use" $ \selfRef _ activatorRef -> do
    modifyRef selfRef (\v -> v & eActivator .~ activatorRef)

    self <- readRef selfRef

    if (self^.eSpawnFlags) .&. trainStartOn /= 0
      then
        unless ((self^.eSpawnFlags) .&. trainToggle == 0) $ do
          modifyRef selfRef (\v -> v & eSpawnFlags %~ (.&. (complement trainStartOn))
                                        & eVelocity .~ V3 0 0 0
                                        & eNextThink .~ 0)

      else
        if isJust (self^.eTargetEnt)
          then trainResume selfRef
          else void $ think trainNext selfRef

funcTrainFind :: EntThink
funcTrainFind =
  GenericEntThink "func_train_find" $ \selfRef -> do
    self <- readRef selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let dprintf = gameImport^.giDprintf
        linkEntity = gameImport^.giLinkEntity

    if isNothing (self^.eTarget)
      then
        dprintf "train_find: no target\n"
      else do
        entRef <- GameBase.pickTarget (self^.eTarget)

        if isNothing entRef
          then
            dprintf $ "train_find: target " `B.append` (fromJust $ self^.eTarget) `B.append` " not found\n"
          else do
            ent <- readRef (fromJust entRef)

            let origin = ent^.eEntityState.esOrigin
                mins = self^.eMins

            modifyRef selfRef (\v -> v & eTarget .~ (ent^.eTarget)
                                          & eEntityState.esOrigin .~ (origin - mins))

            linkEntity selfRef

            -- if not triggered, start immediately
            when (isNothing (self^.eTargetName)) $
              modifyRef selfRef (\v -> v & eSpawnFlags %~ (.|. trainStartOn))

            updatedSelf <- readRef selfRef

            when ((updatedSelf^.eSpawnFlags) .&. trainStartOn /= 0) $ do
              levelTime <- use $ gameBaseGlobals.gbLevel.llTime

              modifyRef selfRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime
                                            & eThink .~ Just trainNext
                                            & eActivator .~ Just selfRef)

    return True

doorUse :: EntUse
doorUse =
  GenericEntUse "door_use" $ \selfRef _ activatorRef -> do
    self <- readRef selfRef

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

  where triggerPairedDoors :: Maybe (Ref EdictT) -> Maybe (Ref EdictT) -> Quake ()
        triggerPairedDoors _ Nothing = return ()
        triggerPairedDoors activatorRef (Just edictRef) = do
          edict <- readRef edictRef
          let teamChain = edict^.eTeamChain

          modifyRef edictRef (\v -> v & eMessage .~ Nothing
                                         & eTouch .~ Nothing)
          doorGoUp edictRef activatorRef
          triggerPairedDoors activatorRef teamChain

doorBlocked :: EntBlocked
doorBlocked =
  GenericEntBlocked "door_blocked" $ \selfRef otherRef -> do
    self <- readRef selfRef
    other <- readRef otherRef
    v3o <- use $ globals.gVec3Origin

    if (other^.eSvFlags) .&. Constants.svfMonster == 0 && isNothing (other^.eClient)
      then do
        -- give it a chance to go away on it's own terms (like gibs)
        GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o 100000 1 0 Constants.modCrush

        -- if it's still there, nuke it
        -- RESEARCH: are we sure it is the correct way? (jake2 has different stuff here)
        other' <- readRef otherRef
        when (other'^.eInUse) $
          GameMisc.becomeExplosion1 otherRef

      else do
        GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

        self' <- readRef selfRef

        unless ((self'^.eSpawnFlags) .&. doorCrusher /= 0) $ do
          -- if a door has a negative wait, it would never come back if
          -- blocked, so let it just squash the object to death real fast
          when ((self'^.eMoveInfo.miWait) >= 0) $ do
            if (self'^.eMoveInfo.miState) == stateDown
              then teamDoorGoUp (self'^.eTeamMaster)
              else teamDoorGoDown (self'^.eTeamMaster)

  where teamDoorGoUp :: Maybe (Ref EdictT) -> Quake ()
        teamDoorGoUp Nothing = return ()
        teamDoorGoUp (Just edictRef) = do
          edict <- readRef edictRef
          doorGoUp edictRef (edict^.eActivator)
          teamDoorGoUp (edict^.eTeamChain)

        teamDoorGoDown :: Maybe (Ref EdictT) -> Quake ()
        teamDoorGoDown Nothing = return ()
        teamDoorGoDown (Just edictRef) = do
          edict <- readRef edictRef
          void $ think doorGoDown edictRef
          teamDoorGoDown (edict^.eTeamChain)

doorKilled :: EntDie
doorKilled =
  GenericEntDie "door_killed" $ \selfRef _ attackerRef _ _ -> do
    self <- readRef selfRef
    updateDoorTeam (self^.eTeamMaster)
    entUse doorUse (fromJust $ self^.eTeamMaster) (Just attackerRef) (Just attackerRef)

  where updateDoorTeam :: Maybe (Ref EdictT) -> Quake ()
        updateDoorTeam Nothing = return ()
        updateDoorTeam (Just edictRef) = do
          edict <- readRef edictRef

          modifyRef edictRef (\v -> v & eHealth .~ (edict^.eMaxHealth)
                                         & eTakeDamage .~ Constants.damageNo)

          updateDoorTeam (edict^.eTeamChain)

doorTouch :: EntTouch
doorTouch =
  GenericEntTouch "door_touch" $ \selfRef otherRef _ _ -> do
    other <- readRef otherRef

    case other^.eClient of
      Nothing ->
        return ()

      Just _ -> do
        self <- readRef selfRef
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        unless (levelTime < (self^.eTouchDebounceTime)) $ do
          modifyRef selfRef (\v -> v & eTouchDebounceTime .~ levelTime + 5.0)

          gameImport <- use $ gameBaseGlobals.gbGameImport

          let centerPrintf = gameImport^.giCenterPrintf
              soundIndex = gameImport^.giSoundIndex
              sound = gameImport^.giSound

          centerPrintf otherRef (fromJust $ self^.eMessage)
          soundIdx <- soundIndex (Just "misc/talk1.wav")
          sound (Just otherRef) Constants.chanAuto soundIdx 1 Constants.attnNorm 0

thinkCalcMoveSpeed :: EntThink
thinkCalcMoveSpeed =
  GenericEntThink "think_calc_movespeed" $ \edictRef -> do
    edict <- readRef edictRef

    -- only the team master does this
    unless ((edict^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
      -- find the smallest distance any member of the team will be moving
      minDist <- findSmallestDistance (edict^.eTeamChain) (abs $ edict^.eMoveInfo.miDistance)

      let time = minDist / (edict^.eMoveInfo.miSpeed)

      -- adjust speeds so they will all complete at the same time
      adjustSpeeds (Just edictRef) time

    return True

  where findSmallestDistance :: Maybe (Ref EdictT) -> Float -> Quake Float
        findSmallestDistance Nothing minDist = return minDist
        findSmallestDistance (Just entRef) minDist = do
          ent <- readRef entRef

          let dist = abs (ent^.eMoveInfo.miDistance)
              minDist' = if dist < minDist then dist else minDist

          findSmallestDistance (ent^.eTeamChain) minDist'

        adjustSpeeds :: Maybe (Ref EdictT) -> Float -> Quake ()
        adjustSpeeds Nothing _ = return ()
        adjustSpeeds (Just entRef) time = do
          ent <- readRef entRef

          let newspeed = (abs $ ent^.eMoveInfo.miSpeed) / time
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
thinkSpawnDoorTrigger =
  GenericEntThink "think_spawn_door_trigger" $ \edictRef -> do
    edict <- readRef edictRef

    -- only the team leader spawns a trigger
    unless ((edict^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
      let mins = edict^.eMins
          maxs = edict^.eMaxs

      (mins', maxs') <- teamChainAddPointToBound (edict^.eTeamChain) mins maxs

      let expandedMins = (V3 (-60) (-60) 0) + mins'
          expandedMaxs = (V3 60 60 0) + maxs'

      otherRef <- GameUtil.spawn

      modifyRef otherRef (\v -> v & eMins .~ expandedMins
                                     & eMaxs .~ expandedMaxs
                                     & eOwner .~ Just edictRef
                                     & eSolid .~ Constants.solidTrigger
                                     & eMoveType .~ Constants.moveTypeNone
                                     & eTouch .~ Just touchDoorTrigger)

      linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
      linkEntity otherRef

      when ((edict^.eSpawnFlags) .&. Constants.doorStartOpen /= 0) $
        doorUseAreaPortals edictRef True
        
      void $ think thinkCalcMoveSpeed edictRef 

    return True

  where teamChainAddPointToBound :: Maybe (Ref EdictT) -> V3 Float -> V3 Float -> Quake (V3 Float, V3 Float)
        teamChainAddPointToBound Nothing mins maxs = return (mins, maxs)
        teamChainAddPointToBound (Just otherRef) mins maxs = do
          other <- readRef otherRef

          let (mins', maxs') = GameBase.addPointToBound (other^.eAbsMin) mins maxs
              (mins'', maxs'') = GameBase.addPointToBound (other^.eAbsMax) mins' maxs'

          teamChainAddPointToBound (other^.eTeamChain) mins'' maxs''

buttonUse :: EntUse
buttonUse =
  GenericEntUse "button_use" $ \selfRef _ activatorRef -> do
    modifyRef selfRef (\v -> v & eActivator .~ activatorRef)
    void $ think buttonFire selfRef

buttonTouch :: EntTouch
buttonTouch =
  GenericEntTouch "button_touch" $ \selfRef otherRef _ _ -> do
    other <- readRef otherRef

    unless (isNothing (other^.eClient) || (other^.eHealth) <= 0) $ do
      modifyRef selfRef (\v -> v & eActivator .~ Just otherRef)
      void $ think buttonFire selfRef

buttonKilled :: EntDie
buttonKilled =
  GenericEntDie "button_killed" $ \selfRef _ attackerRef _ _ -> do
    self <- readRef selfRef

    modifyRef selfRef (\v -> v & eActivator .~ Just attackerRef
                                  & eHealth .~ (self^.eMaxHealth)
                                  & eTakeDamage .~ Constants.damageNo)

    void $ think buttonFire selfRef

buttonFire :: EntThink
buttonFire =
  GenericEntThink "button_fire" $ \selfRef -> do
    self <- readRef selfRef

    if (self^.eMoveInfo.miState) == stateUp || (self^.eMoveInfo.miState) == stateTop
      then
        return True

      else do
        modifyRef selfRef (\v -> v & eMoveInfo.miState .~ stateUp)

        when ((self^.eMoveInfo.miSoundStart) /= 0 && (self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
          sound <- use $ gameBaseGlobals.gbGameImport.giSound
          sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0

        moveCalc selfRef (self^.eMoveInfo.miEndOrigin) buttonWait
        return True

buttonWait :: EntThink
buttonWait =
  GenericEntThink "button_wait" $ \selfRef -> do
    self <- readRef selfRef

    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ stateTop
                                  & eEntityState.esEffects %~ (.&. (complement Constants.efAnim01))
                                  & eEntityState.esEffects %~ (.|. Constants.efAnim23))

    GameUtil.useTargets selfRef (self^.eActivator)

    modifyRef selfRef (\v -> v & eEntityState.esFrame .~ 1)

    self' <- readRef selfRef

    when ((self'^.eMoveInfo.miWait) >= 0) $ do
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      modifyRef selfRef (\v -> v & eNextThink .~ levelTime + (self'^.eMoveInfo.miWait)
                                    & eThink .~ Just buttonReturn)

    return True

buttonReturn :: EntThink
buttonReturn =
  GenericEntThink "button_return" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ stateDown)

    self <- readRef selfRef

    moveCalc selfRef (self^.eMoveInfo.miStartOrigin) buttonDone

    modifyRef selfRef (\v -> v & eEntityState.esFrame .~ 0)

    self' <- readRef selfRef

    when ((self'^.eHealth) /= 0) $
      modifyRef selfRef (\v -> v & eTakeDamage .~ Constants.damageYes)

    return True

buttonDone :: EntThink
buttonDone =
  GenericEntThink "button_done" $ \selfRef -> do
    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ stateBottom
                                  & eEntityState.esEffects %~ (.&. (complement Constants.efAnim23))
                                  & eEntityState.esEffects %~ (.|. Constants.efAnim01))

    return True

trainNext :: EntThink
trainNext =
  GenericEntThink "train_next" $ \edictRef -> do
    (done, entRef) <- pickNextTarget edictRef True

    unless done $ do
      edict <- readRef edictRef
      ent <- readRef (fromJust entRef)

      modifyRef edictRef (\v -> v & eMoveInfo.miWait .~ (ent^.eMoveInfo.miWait)
                                     & eTargetEnt .~ entRef)

      when ((edict^.eFlags) .&. Constants.flTeamSlave == 0) $ do
        when ((edict^.eMoveInfo.miSoundStart) /= 0) $ do
          sound <- use $ gameBaseGlobals.gbGameImport.giSound
          sound (Just edictRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (edict^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0

        modifyRef edictRef (\v -> v & eEntityState.esSound .~ (edict^.eMoveInfo.miSoundMiddle))

      let dest = (ent^.eEntityState.esOrigin) - (edict^.eMins)

      modifyRef edictRef (\v -> v & eMoveInfo.miState .~ Constants.stateTop
                                     & eMoveInfo.miStartOrigin .~ (edict^.eEntityState.esOrigin)
                                     & eMoveInfo.miEndOrigin .~ dest)

      moveCalc edictRef dest trainWait

      modifyRef edictRef (\v -> v & eSpawnFlags %~ (.|. trainStartOn))

    return True

  where pickNextTarget :: Ref EdictT -> Bool -> Quake (Bool, Maybe (Ref EdictT))
        pickNextTarget selfRef first = do
          self <- readRef selfRef
          gameImport <- use $ gameBaseGlobals.gbGameImport

          let dprintf = gameImport^.giDprintf
              linkEntity = gameImport^.giLinkEntity

          if isNothing (self^.eTarget)
            then
              return (True, Nothing)

            else do
              entRef <- GameBase.pickTarget (self^.eTarget)

              if isNothing entRef
                then do
                  dprintf $ "train_next: bad target " `B.append` (fromJust $ self^.eTarget) `B.append` "\n"
                  return (True, Nothing)
                else do
                  ent <- readRef (fromJust entRef)
                  modifyRef selfRef (\v -> v & eTarget .~ (ent^.eTarget))

                  -- check for a teleport path_corner
                  if (ent^.eSpawnFlags) .&. 1 /= 0
                    then
                      if not first
                        then do
                          dprintf $ "connected teleport path_corner, see " `B.append` (ent^.eClassName) `B.append` " at " `B.append` (Lib.vtos (ent^.eEntityState.esOrigin)) `B.append` "\n"
                          return (True, entRef)
                        else do
                          let origin = ((ent^.eEntityState.esOrigin) - (self^.eMins))

                          modifyRef selfRef (\v -> v & eEntityState.esOrigin .~ origin
                                                        & eEntityState.esOldOrigin .~ origin
                                                        & eEntityState.esEvent .~ Constants.evOtherTeleport)

                          linkEntity selfRef

                          pickNextTarget selfRef False

                    else return (False, entRef)

touchDoorTrigger :: EntTouch
touchDoorTrigger =
  GenericEntTouch "touch_door_trigger" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef
    other <- readRef otherRef

    let Just ownerRef = self^.eOwner
    owner <- readRef ownerRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let skip = (other^.eHealth) <= 0 
            || ((other^.eSvFlags) .&. Constants.svfMonster == 0 && isNothing (other^.eClient))
            || ((owner^.eSpawnFlags) .&. Constants.doorNoMonster /= 0 && (other^.eSvFlags) .&. Constants.svfMonster /= 0)
            || levelTime < (self^.eTouchDebounceTime)

    unless skip $ do
      modifyRef selfRef (\v -> v & eTouchDebounceTime .~ levelTime + 1)
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

doorUseAreaPortals :: Ref EdictT -> Bool -> Quake ()
doorUseAreaPortals selfRef open = do
    self <- readRef selfRef

    case self^.eTarget of
      Nothing -> return ()
      Just target -> setAreaPortals target Nothing

  where setAreaPortals :: B.ByteString -> Maybe (Ref EdictT) -> Quake ()
        setAreaPortals target ref = do
          maybeFoundRef <- GameBase.gFind ref GameBase.findByTarget target

          case maybeFoundRef of
            Nothing -> return ()
            Just foundRef -> do
              foundEdict <- readRef foundRef
              when (BC.map toLower (foundEdict^.eClassName) == "func_areaportal") $ do
                setAreaPortalState <- use $ gameBaseGlobals.gbGameImport.giSetAreaPortalState
                setAreaPortalState (foundEdict^.eStyle) open
              setAreaPortals target maybeFoundRef

trainResume :: Ref EdictT -> Quake ()
trainResume selfRef = do
    self <- readRef selfRef

    let Just edictRef = self^.eTargetEnt

    edict <- readRef edictRef

    let dest = (edict^.eEntityState.esOrigin) - (self^.eMins)

    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ stateTop
                                  & eMoveInfo.miStartOrigin .~ (self^.eEntityState.esOrigin)
                                  & eMoveInfo.miEndOrigin .~ dest)

    moveCalc selfRef dest trainWait

    modifyRef selfRef (\v -> v & eSpawnFlags %~ (.|. trainStartOn))

trainWait :: EntThink
trainWait =
  GenericEntThink "train_wait" $ \edictRef -> do
    done <- checkPathTarget edictRef

    unless done $ do
      edict <- readRef edictRef

      if edict^.eMoveInfo.miWait /= 0
        then do
          if | edict^.eMoveInfo.miWait > 0 -> do
                 levelTime <- use $ gameBaseGlobals.gbLevel.llTime

                 modifyRef edictRef (\v -> v & eNextThink .~ levelTime + (edict^.eMoveInfo.miWait)
                                                & eThink .~ Just trainNext)

             | (edict^.eSpawnFlags) .&. trainToggle /= 0 -> do
                 void $ think trainNext edictRef

                 modifyRef edictRef (\v -> v & eSpawnFlags %~ (.&. (complement trainStartOn))
                                                & eVelocity .~ V3 0 0 0
                                                & eNextThink .~ 0)

             | otherwise -> return ()

          when ((edict^.eFlags) .&. Constants.flTeamSlave == 0) $ do
            when ((edict^.eMoveInfo.miSoundEnd) /= 0) $ do
              sound <- use $ gameBaseGlobals.gbGameImport.giSound
              sound (Just edictRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (edict^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0

            modifyRef edictRef (\v -> v & eEntityState.esSound .~ 0)

        else void $ think trainNext edictRef

    return True

  where checkPathTarget :: Ref EdictT -> Quake Bool
        checkPathTarget edictRef = do
          edict <- readRef edictRef
          let Just targetRef = edict^.eTargetEnt
          target <- readRef targetRef

          if isJust $ target^.ePathTarget
            then do
              let saveTarget = target^.eTarget
              modifyRef targetRef (\v -> v & eTarget .~ (target^.ePathTarget))

              GameUtil.useTargets targetRef (edict^.eActivator)

              modifyRef targetRef (\v -> v & eTarget .~ saveTarget)

              -- make sure we didn't get killed by a killtarget
              edict' <- readRef edictRef
              let inUse = edict'^.eInUse
              return $ if inUse then False else True

            else return False

moveCalc :: Ref EdictT -> V3 Float -> EntThink -> Quake ()
moveCalc edictRef dest func = do
    modifyRef edictRef (\v -> v & eVelocity .~ V3 0 0 0)

    edict <- readRef edictRef

    let dir = dest - (edict^.eEntityState.esOrigin)

    modifyRef edictRef (\v -> v & eMoveInfo.miDir .~ normalize dir
                                   & eMoveInfo.miRemainingDistance .~ norm dir
                                   & eMoveInfo.miEndFunc .~ Just func)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    if (edict^.eMoveInfo.miSpeed) == (edict^.eMoveInfo.miAccel) && (edict^.eMoveInfo.miSpeed) == (edict^.eMoveInfo.miDecel)
      then do
        currentEntity <- use $ gameBaseGlobals.gbLevel.llCurrentEntity

        let comparedEntity = if (edict^.eFlags) .&. Constants.flTeamSlave /= 0
                               then edict^.eTeamMaster
                               else Just edictRef

        if currentEntity == comparedEntity
          then
            void $ think moveBegin edictRef

          else
            modifyRef edictRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime
                                           & eThink .~ Just moveBegin)

      else do
        -- aceelerative
        modifyRef edictRef (\v -> v & eMoveInfo.miCurrentSpeed .~ 0
                                       & eThink .~ Just thinkAccelMove
                                       & eNextThink .~ levelTime + Constants.frameTime)

angleMoveCalc :: Ref EdictT -> EntThink -> Quake ()
angleMoveCalc edictRef func = do
    modifyRef edictRef (\v -> v & eAVelocity .~ V3 0 0 0
                                   & eMoveInfo.miEndFunc .~ Just func)

    currentEntity <- use $ gameBaseGlobals.gbLevel.llCurrentEntity

    edict <- readRef edictRef
    let ent = if (edict^.eFlags) .&. Constants.flTeamSlave /= 0
                then edict^.eTeamMaster
                else Just edictRef

    if currentEntity == ent
      then
        void $ think angleMoveBegin edictRef

      else do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        modifyRef edictRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime
                                       & eThink .~ Just angleMoveBegin)

angleMoveBegin :: EntThink
angleMoveBegin =
  GenericEntThink "angle_move_begin" $ \edictRef -> do
    edict <- readRef edictRef

        -- set destdelta to the vector needed to move
    let destDelta = if (edict^.eMoveInfo.miState) == stateUp
                      then (edict^.eMoveInfo.miEndAngles) - (edict^.eEntityState.esAngles)
                      else (edict^.eMoveInfo.miStartAngles) - (edict^.eEntityState.esAngles)
        -- calculate length of vector
        len = norm destDelta
        -- divide by speed to get time to reach dest
        travelTime = len / (edict^.eMoveInfo.miSpeed)

    if travelTime < Constants.frameTime
      then do
        void $ think angleMoveFinal edictRef
        return True

      else do
        let frames = floor (travelTime / Constants.frameTime) :: Int
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        -- scale the destdelta vector by the time spent traveling to get
        -- velocity
        modifyRef edictRef (\v -> v & eAVelocity .~ fmap (* (1.0 / travelTime)) destDelta
                                       -- set nextthink to trigger a think when dest
                                       -- is reached
                                       & eNextThink .~ levelTime + fromIntegral frames * Constants.frameTime
                                       & eThink .~ Just angleMoveFinal)

        return True

angleMoveFinal :: EntThink
angleMoveFinal =
  GenericEntThink "angle_move_final" $ \edictRef -> do
    edict <- readRef edictRef
    v3o <- use $ globals.gVec3Origin
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let move = if (edict^.eMoveInfo.miState) == stateUp
                 then (edict^.eMoveInfo.miEndAngles) - (edict^.eEntityState.esAngles)
                 else (edict^.eMoveInfo.miStartAngles) - (edict^.eEntityState.esAngles)

    if move == v3o
      then do
        void $ think angleMoveDone edictRef
        return True

      else do
        modifyRef edictRef (\v -> v & eAVelocity .~ fmap (* (1.0 / Constants.frameTime)) move
                                       & eThink .~ Just angleMoveDone
                                       & eNextThink .~ levelTime + Constants.frameTime)
        return True

angleMoveDone :: EntThink
angleMoveDone =
  GenericEntThink "angle_move_done" $ \edictRef -> do
    modifyRef edictRef (\v -> v & eAVelocity .~ V3 0 0 0)

    edict <- readRef edictRef
    void $ think (fromJust $ edict^.eMoveInfo.miEndFunc) edictRef

    return True

moveBegin :: EntThink
moveBegin =
  GenericEntThink "move_begin" $ \edictRef -> do
    edict <- readRef edictRef
    let moveInfo = edict^.eMoveInfo

    if (moveInfo^.miSpeed) * Constants.frameTime >= (moveInfo^.miRemainingDistance)
      then do
        void $ think moveFinal edictRef
        return True

      else do
        let velocity = fmap (* (moveInfo^.miSpeed)) (moveInfo^.miDir)
            frames = floor $ ((moveInfo^.miRemainingDistance) / (moveInfo^.miSpeed)) / Constants.frameTime :: Int
            framesF = fromIntegral frames :: Float

        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyRef edictRef (\v -> v & eVelocity .~ velocity
                                       & eMoveInfo.miRemainingDistance -~ framesF * (moveInfo^.miSpeed) * Constants.frameTime
                                       & eNextThink .~ levelTime + (framesF * Constants.frameTime)
                                       & eThink .~ Just moveFinal)

        return True

thinkAccelMove :: EntThink
thinkAccelMove =
  GenericEntThink "think_accelmove" $ \edictRef -> do
    edict <- readRef edictRef

    modifyRef edictRef (\v -> v & eMoveInfo.miRemainingDistance -~ (edict^.eMoveInfo.miCurrentSpeed))

    when ((edict^.eMoveInfo.miCurrentSpeed) == 0) $ -- starting or blocked
      platCalcAcceleratedMove edictRef

    platAccelerate edictRef

    -- will the entire move complete on next frame?
    edict' <- readRef edictRef

    if (edict'^.eMoveInfo.miRemainingDistance) <= (edict'^.eMoveInfo.miCurrentSpeed)
      then do
        void $ think moveFinal edictRef
        return True

      else do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyRef edictRef (\v -> v & eVelocity .~ fmap (* ((edict'^.eMoveInfo.miCurrentSpeed) * 10)) (edict'^.eMoveInfo.miDir)
                                       & eNextThink .~ levelTime + Constants.frameTime
                                       & eThink .~ Just thinkAccelMove)
        return True

platAccelerate :: Ref EdictT -> Quake ()
platAccelerate edictRef = do
    edict <- readRef edictRef
    let moveInfo = edict^.eMoveInfo

         -- are we decelerating?
    if | (moveInfo^.miRemainingDistance) <= (moveInfo^.miDecelDistance) -> do
           when ((moveInfo^.miRemainingDistance) < (moveInfo^.miDecelDistance)) $
             if (moveInfo^.miNextSpeed) /= 0
               then
                 modifyRef edictRef (\v -> v & eMoveInfo.miCurrentSpeed .~ (moveInfo^.miNextSpeed)
                                                & eMoveInfo.miNextSpeed .~ 0)

               else
                 when ((moveInfo^.miCurrentSpeed) > (moveInfo^.miDecel)) $
                   modifyRef edictRef (\v -> v & eMoveInfo.miCurrentSpeed -~ (moveInfo^.miDecel))

         -- are we at full speed and need to start decelerating during this move?
       | (moveInfo^.miCurrentSpeed) == (moveInfo^.miMoveSpeed) && (moveInfo^.miRemainingDistance) - (moveInfo^.miCurrentSpeed) < (moveInfo^.miDecelDistance) -> do
           let p1Distance = (moveInfo^.miRemainingDistance) - (moveInfo^.miDecelDistance)
               p2Distance = (moveInfo^.miMoveSpeed) * (1.0 - (p1Distance / (moveInfo^.miMoveSpeed)))
               distance = p1Distance + p2Distance

           modifyRef edictRef (\v -> v & eMoveInfo.miCurrentSpeed .~ (moveInfo^.miMoveSpeed)
                                          & eMoveInfo.miNextSpeed .~ (moveInfo^.miMoveSpeed) - (moveInfo^.miDecel) * (p2Distance / distance))

         -- are we accelerating?
       | (moveInfo^.miCurrentSpeed) < (moveInfo^.miSpeed) -> do
           let oldSpeed = moveInfo^.miCurrentSpeed
               -- figure simple acceleration up to move_speed
               speed = (moveInfo^.miCurrentSpeed) + (moveInfo^.miAccel)
               currentSpeed = if speed > (moveInfo^.miSpeed)
                                then moveInfo^.miSpeed
                                else speed

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
       | otherwise ->
           return ()

platCalcAcceleratedMove :: Ref EdictT -> Quake ()
platCalcAcceleratedMove edictRef = do
    readRef edictRef >>= \edict ->
      modifyRef edictRef (\v -> v & eMoveInfo.miMoveSpeed .~ (edict^.eMoveInfo.miSpeed))

    edict <- readRef edictRef
    let moveInfo = edict^.eMoveInfo

    if (moveInfo^.miRemainingDistance) < (moveInfo^.miAccel)
      then
        modifyRef edictRef (\v -> v & eMoveInfo.miCurrentSpeed .~ (moveInfo^.miRemainingDistance))

      else do
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

  where accelerationDistance :: Float -> Float -> Float
        accelerationDistance target rate = target * ((target / rate) + 1) / 2

moveFinal :: EntThink
moveFinal =
  GenericEntThink "move_final" $ \edictRef -> do
    edict <- readRef edictRef

    if (edict^.eMoveInfo.miRemainingDistance) == 0
      then do
        void $ think moveDone edictRef
        return True
      else do
        let velocity = fmap (* ((edict^.eMoveInfo.miRemainingDistance) / Constants.frameTime)) (edict^.eMoveInfo.miDir)
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        
        modifyRef edictRef (\v -> v & eVelocity .~ velocity
                                       & eThink .~ Just moveDone
                                       & eNextThink .~ levelTime + Constants.frameTime)

        return True

moveDone :: EntThink
moveDone =
  GenericEntThink "move_done" $ \edictRef -> do
    modifyRef edictRef (\v -> v & eVelocity .~ V3 0 0 0)
    edict <- readRef edictRef
    let endFunc = edict^.eMoveInfo.miEndFunc

    void $ think (fromJust endFunc) edictRef

    return True

rotatingUse :: EntUse
rotatingUse =
  GenericEntUse "rotating_use" $ \selfRef _ _ -> do
    self <- readRef selfRef
    vec3origin <- use $ globals.gVec3Origin

    if (self^.eAVelocity) /= vec3origin
      then
        modifyRef selfRef (\v -> v & eEntityState.esSound .~ 0
                                      & eAVelocity .~ V3 0 0 0
                                      & eTouch .~ Nothing)

      else do
        modifyRef selfRef (\v -> v & eEntityState.esSound .~ (self^.eMoveInfo.miSoundMiddle)
                                      & eAVelocity .~ fmap (* (self^.eSpeed)) (self^.eMoveDir))

        when ((self^.eSpawnFlags) .&. 16 /= 0) $
          modifyRef selfRef (\v -> v & eTouch .~ Just rotatingTouch)

rotatingBlocked :: EntBlocked
rotatingBlocked =
  GenericEntBlocked "rotating_blocked" $ \selfRef otherRef -> do
    self <- readRef selfRef
    other <- readRef otherRef
    v3o <- use $ globals.gVec3Origin

    GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

rotatingTouch :: EntTouch
rotatingTouch =
  GenericEntTouch "rotating_touch" $ \selfRef otherRef _ _ -> do
    self <- readRef selfRef

    when ((self^.eAVelocity._x) /= 0 || (self^.eAVelocity._y) /= 0 || (self^.eAVelocity._z) /= 0) $ do
      other <- readRef otherRef
      v3o <- use $ globals.gVec3Origin

      GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

doorGoUp :: Ref EdictT -> Maybe (Ref EdictT) -> Quake ()
doorGoUp selfRef activatorRef = do
    self <- readRef selfRef

    unless ((self^.eMoveInfo.miState) == Constants.stateUp) $ do
      if (self^.eMoveInfo.miState) == Constants.stateTop
        then do
          -- reset top wait time
          when ((self^.eMoveInfo.miWait) >= 0) $ do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime
            modifyRef selfRef (\v -> v & eNextThink .~ levelTime + (self^.eMoveInfo.miWait))

        else do
          when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
            when ((self^.eMoveInfo.miSoundStart) /= 0) $ do
              sound <- use $ gameBaseGlobals.gbGameImport.giSound
              sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0

            modifyRef selfRef (\v -> v & eEntityState.esSound .~ self^.eMoveInfo.miSoundMiddle)

          modifyRef selfRef (\v -> v & eMoveInfo.miState .~ Constants.stateUp)

          if | (self^.eClassName) == "func_door" -> moveCalc selfRef (self^.eMoveInfo.miEndOrigin) doorHitTop
             | (self^.eClassName) == "func_door_rotating" -> angleMoveCalc selfRef doorHitTop
             | otherwise -> return ()

          GameUtil.useTargets selfRef activatorRef
          doorUseAreaPortals selfRef True

doorHitTop :: EntThink
doorHitTop =
  GenericEntThink "door_hit_top" $ \selfRef -> do
    self <- readRef selfRef

    when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((self^.eMoveInfo.miSoundEnd) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0

      modifyRef selfRef (\v -> v & eEntityState.esSound .~ 0)

    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ Constants.stateTop)

    if (self^.eSpawnFlags) .&. Constants.doorToggle /= 0
      then
        return True

      else do
        when ((self^.eMoveInfo.miWait) >= 0) $ do
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime

          modifyRef selfRef (\v -> v & eThink .~ Just doorGoDown
                                        & eNextThink .~ levelTime + (self^.eMoveInfo.miWait))

        return True

doorGoDown :: EntThink
doorGoDown =
  GenericEntThink "door_go_down" $ \selfRef -> do
    self <- readRef selfRef

    when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((self^.eMoveInfo.miSoundStart) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0

      modifyRef selfRef (\v -> v & eEntityState.esSound .~ self^.eMoveInfo.miSoundMiddle)

    when ((self^.eMaxHealth) /= 0) $ do
      modifyRef selfRef (\v -> v & eTakeDamage .~ Constants.damageYes
                                    & eHealth .~ self^.eMaxHealth)

    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ Constants.stateDown)

    if | (self^.eClassName) == "func_door" -> moveCalc selfRef (self^.eMoveInfo.miStartOrigin) doorHitBottom
       | (self^.eClassName) == "func_door_rotating" -> angleMoveCalc selfRef doorHitBottom
       | otherwise -> return ()

    return True

doorHitBottom :: EntThink
doorHitBottom =
  GenericEntThink "door_hit_bottom" $ \selfRef -> do
    self <- readRef selfRef

    when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((self^.eMoveInfo.miSoundEnd) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0
      
      modifyRef selfRef (\v -> v & eEntityState.esSound .~ 0)

    modifyRef selfRef (\v -> v & eMoveInfo.miState .~ Constants.stateBottom)
    doorUseAreaPortals selfRef False
    return True

{-
- QUAKED func_conveyor (0 .5 .8) ? START_ON TOGGLE Conveyors are stationary
- brushes that move what's on them. The brush should be have a surface with
- at least one current content enabled. speed default 100
-}
funcConveyorUse :: EntUse
funcConveyorUse =
  GenericEntUse "func_conveyor_use" $ \selfRef _ _ -> do
    self <- readRef selfRef

    if (self^.eSpawnFlags) .&. 1 /= 0
      then do
        modifyRef selfRef (\v -> v & eSpeed .~ 0
                                      & eSpawnFlags %~ (.&. (complement 1)))
      else do
        modifyRef selfRef (\v -> v & eSpeed .~ fromIntegral (self^.eCount)
                                      & eSpawnFlags %~ (.|. 1))

    self' <- readRef selfRef
    let spawnFlags = self'^.eSpawnFlags
    when (spawnFlags .&. 2 == 0) $
      modifyRef selfRef (\v -> v & eCount .~ 0)

triggerElevatorUse :: EntUse
triggerElevatorUse =
  GenericEntUse "trigger_elevator_use" $ \selfRef (Just otherRef) _ -> do
    self <- readRef selfRef
    let Just moveTargetRef = self^.eMoveTarget
    moveTarget <- readRef moveTargetRef

    unless ((moveTarget^.eNextThink) /= 0) $ do
      other <- readRef otherRef
      dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf

      case other^.ePathTarget of
        Nothing ->
          dprintf "elevator used with no pathtarget\n"

        Just pathTarget -> do
          foundTarget <- GameBase.pickTarget (other^.ePathTarget)

          case foundTarget of
            Nothing ->
              dprintf ("elevator used with bad pathtarget: " `B.append` pathTarget `B.append` "\n")

            Just targetRef -> do
              modifyRef moveTargetRef (\v -> v & eTargetEnt .~ foundTarget)
              trainResume moveTargetRef
