{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameFunc where

import Control.Lens (use, preuse, (.=), (^.), ix, zoom, (%=), (-=), (&), (.~), (%~), (-~))
import Control.Monad (when, liftM, void, unless)
import Data.Bits ((.&.), (.|.), complement)
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, isNothing)
import Linear (V3(..), _x, _y, _z, normalize, quadrance, dot)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
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

    edict <- readEdictT edictRef

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeStop
                                   & eSolid .~ Constants.solidBsp)

    setModel edictRef (edict^.eiModel)

    when ((edict^.eSounds) /= 1) $ do
      soundIdx <- soundIndex (Just "switches/butn2.wav")
      modifyEdictT edictRef (\v -> v & eMoveInfo.miSoundStart .~ soundIdx)

    let speed = if (edict^.eSpeed) == 0 then 40 else edict^.eSpeed
    modifyEdictT edictRef (\v -> v & eSpeed .~ speed)

    when ((edict^.eAccel) == 0) $
      modifyEdictT edictRef (\v -> v & eAccel .~ speed)

    when ((edict^.eDecel) == 0) $
      modifyEdictT edictRef (\v -> v & eDecel .~ speed)

    when ((edict^.eWait) == 0) $
      modifyEdictT edictRef (\v -> v & eWait .~ 3)
    
    lip <- use $ gameBaseGlobals.gbSpawnTemp.stLip
    when (lip == 0) $
      gameBaseGlobals.gbSpawnTemp.stLip .= 4

    let origin = edict^.eEntityState.esOrigin
    modifyEdictT edictRef (\v -> v & ePos1 .~ origin)

    lip' <- use $ gameBaseGlobals.gbSpawnTemp.stLip
    let moveDir = edict^.eMoveDir
        absMoveDir = fmap abs moveDir
        size = edict^.eSize
        dist = (absMoveDir^._x) * (size^._x)
             + (absMoveDir^._y) * (size^._y)
             + (absMoveDir^._z) * (size^._z)
             - (fromIntegral lip')

    let pos2 = origin + fmap (* dist) moveDir
    modifyEdictT edictRef (\v -> v & ePos2 .~ pos2
                                   & eUse .~ Just buttonUse
                                   & eEntityState.esEffects %~ (.|. Constants.efAnim01))

    if (edict^.eHealth) /= 0
      then
        modifyEdictT edictRef (\v -> v & eMaxHealth .~ (edict^.eHealth)
                                       & eDie .~ Just buttonKilled
                                       & eTakeDamage .~ Constants.damageYes)

      else
        when (isNothing (edict^.eTargetName)) $
          modifyEdictT edictRef (\v -> v & eTouch .~ Just buttonTouch)

    updatedEdict <- readEdictT edictRef

    modifyEdictT edictRef (\v -> v & eMoveInfo.miState .~ Constants.stateBottom
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

    edict <- readEdictT edictRef

    when ((edict^.eSounds) /= 1) $ do
      soundStart <- soundIndex (Just "doors/dr1_strt.wav")
      soundMiddle <- soundIndex (Just "doors/dr1_mid.wav")
      soundEnd <- soundIndex (Just "doors/dr1_end.wav")

      modifyEdictT edictRef (\v -> v & eMoveInfo.miSoundStart .~ soundStart
                                     & eMoveInfo.miSoundMiddle .~ soundMiddle
                                     & eMoveInfo.miSoundEnd .~ soundEnd)

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                   & eSolid .~ Constants.solidBsp)

    setModel edictRef (edict^.eiModel)

    modifyEdictT edictRef (\v -> v & eBlocked .~ Just doorBlocked
                                   & eUse .~ Just doorUse)

    deathmatchValue <- liftM (^.cvValue) deathmatchCVar
    let speed = if (edict^.eSpeed) == 0 then 100 else edict^.eSpeed
        speed' = if deathmatchValue /= 0 then speed * 2 else speed

    modifyEdictT edictRef (\v -> v & eSpeed .~ speed')

    when ((edict^.eAccel) == 0) $
      modifyEdictT edictRef (\v -> v & eAccel .~ speed')

    when ((edict^.eDecel) == 0) $
      modifyEdictT edictRef (\v -> v & eDecel .~ speed')

    when ((edict^.eWait) == 0) $
      modifyEdictT edictRef (\v -> v & eWait .~ 3)

    when ((edict^.eDmg) == 0) $
      modifyEdictT edictRef (\v -> v & eDmg .~ 2)
    
    lip <- use $ gameBaseGlobals.gbSpawnTemp.stLip
    when (lip == 0) $
      gameBaseGlobals.gbSpawnTemp.stLip .= 8

    -- calculate second position
    let origin = edict^.eEntityState.esOrigin
    modifyEdictT edictRef (\v -> v & ePos1 .~ origin)

    lip' <- use $ gameBaseGlobals.gbSpawnTemp.stLip
    let moveDir = edict^.eMoveDir
        absMoveDir = fmap abs moveDir
        size = edict^.eSize
        dist = (absMoveDir^._x) * (size^._x)
             + (absMoveDir^._y) * (size^._y)
             + (absMoveDir^._z) * (size^._z)
             - (fromIntegral lip')

    modifyEdictT edictRef (\v -> v & eMoveInfo.miDistance .~ dist)

    let pos2 = origin + fmap (* dist) moveDir
    modifyEdictT edictRef (\v -> v & ePos2 .~ pos2)

    -- if it starts open, switch the positions
    when ((edict^.eSpawnFlags) .&. Constants.doorStartOpen /= 0) $ do
      modifyEdictT edictRef (\v -> v & eEntityState.esOrigin .~ pos2
                                     & ePos2 .~ origin
                                     & ePos1 .~ pos2)

    modifyEdictT edictRef (\v -> v & eMoveInfo.miState .~ Constants.stateBottom)

    if (edict^.eHealth) /= 0
      then
        modifyEdictT edictRef (\v -> v & eTakeDamage .~ Constants.damageYes
                                       & eMaxHealth .~ (edict^.eHealth)
                                       & eDie .~ Just doorKilled)

      else
        when (isJust (edict^.eTargetName) && isJust (edict^.eMessage)) $ do
          void $ soundIndex (Just "misc/talk.wav")
          modifyEdictT edictRef (\v -> v & eTouch .~ Just doorTouch)

    updatedEdict <- readEdictT edictRef

    modifyEdictT edictRef (\v -> v & eMoveInfo.miSpeed .~ (updatedEdict^.eSpeed)
                                   & eMoveInfo.miAccel .~ (updatedEdict^.eAccel)
                                   & eMoveInfo.miDecel .~ (updatedEdict^.eDecel)
                                   & eMoveInfo.miWait .~ (updatedEdict^.eWait)
                                   & eMoveInfo.miStartOrigin .~ (updatedEdict^.ePos1)
                                   & eMoveInfo.miStartAngles .~ (updatedEdict^.eEntityState.esAngles)
                                   & eMoveInfo.miEndOrigin .~ (updatedEdict^.ePos2)
                                   & eMoveInfo.miEndAngles .~ (updatedEdict^.eEntityState.esAngles))

    when ((updatedEdict^.eSpawnFlags) .&. 16 /= 0) $
      modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAll))

    when ((updatedEdict^.eSpawnFlags) .&. 64 /= 0) $
      modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAllFast))

    -- to simplify logic elsewhere, make non-teamed doors into a team of one
    when (isNothing (updatedEdict^.eTeam)) $
      modifyEdictT edictRef (\v -> v & eTeamMaster .~ Just edictRef)

    linkEntity edictRef

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyEdictT edictRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime)

    let nextThink = if (updatedEdict^.eHealth) /= 0 || isJust (updatedEdict^.eTargetName)
                      then thinkCalcMoveSpeed
                      else thinkSpawnDoorTrigger

    modifyEdictT edictRef (\v -> v & eThink .~ Just nextThink)

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

    modifyEdictT edictRef (\v -> v & eMoveInfo.miSoundStart .~ soundStart
                                   & eMoveInfo.miSoundMiddle .~ soundMiddle
                                   & eMoveInfo.miSoundEnd .~ soundEnd
                                   & eMoveType .~ Constants.moveTypePush
                                   & eSolid .~ Constants.solidBsp
                                   & eBlocked .~ Just doorSecretBlocked
                                   & eUse .~ Just doorSecretUse)

    edict <- readEdictT edictRef

    setModel edictRef (edict^.eiModel)

    when (isNothing (edict^.eTargetName) || (edict^.eSpawnFlags) .&. secretAlwaysShoot /= 0) $
      modifyEdictT edictRef (\v -> v & eHealth .~ 0
                                     & eTakeDamage .~ Constants.damageYes
                                     & eDie .~ Just doorSecretDie)

    when ((edict^.eDmg) == 0) $
      modifyEdictT edictRef (\v -> v & eDmg .~ 2)

    when ((edict^.eWait) == 0) $
      modifyEdictT edictRef (\v -> v & eWait .~ 5)


    modifyEdictT edictRef (\v -> v & eMoveInfo.miAccel .~ 50
                                   & eMoveInfo.miDecel .~ 50
                                   & eMoveInfo.miSpeed .~ 50)

    -- calculate positions
    let (Just forward, Just right, Just up) = Math3D.angleVectors (edict^.eEntityState.esAngles) True True True

    modifyEdictT edictRef (\v -> v & eEntityState.esAngles .~ V3 0 0 0)

    let side = 1.0 - fromIntegral ((edict^.eSpawnFlags) .&. secretFirstLeft)
        width = if (edict^.eSpawnFlags) .&. secretFirstDown /= 0
                  then abs (up `dot` (edict^.eSize))
                  else abs (right `dot` (edict^.eSize))
        length = abs (forward `dot` (edict^.eSize))
        pos1 = if (edict^.eSpawnFlags) .&. secretFirstDown /= 0
                 then (edict^.eEntityState.esOrigin) + fmap (* ((-1) * width)) up
                 else (edict^.eEntityState.esOrigin) + fmap (* (side * width)) right
        pos2 = pos1 + fmap (* length) forward

    modifyEdictT edictRef (\v -> v & ePos1 .~ pos1
                                   & ePos2 .~ pos2)

    if (edict^.eHealth) /= 0
      then
        modifyEdictT edictRef (\v -> v & eTakeDamage .~ Constants.damageYes
                                       & eDie .~ Just doorKilled
                                       & eMaxHealth .~ (edict^.eHealth))
      else do
        void $ soundIndex (Just "misc/talk.wav")
        modifyEdictT edictRef (\v -> v & eTouch .~ Just doorTouch)

    modifyEdictT edictRef (\v -> v & eClassName .~ "func_door")

    linkEntity edictRef
    return True

doorSecretBlocked :: EntBlocked
doorSecretBlocked =
  GenericEntBlocked "door_secret_blocked" $ \selfRef otherRef -> do
    self <- readEdictT selfRef
    other <- readEdictT otherRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    v3o <- use $ globals.vec3Origin

    if | (other^.eSvFlags) .&. Constants.svfMonster == 0 && isNothing (other^.eClient) -> do
           -- give it a chance to go away on it's own terms (like gibs)
           GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o 100000 1 0 Constants.modCrush
           -- if it's still there, nuke it
           -- TODO: are we sure it is the correct way? (jake2 has different stuff here)
           other' <- readEdictT otherRef
           when (other'^.eInUse) $
             GameMisc.becomeExplosion1 otherRef

       | levelTime < (self^.eTouchDebounceTime) ->
           return ()

       | otherwise -> do
           modifyEdictT selfRef (\v -> v & eTouchDebounceTime .~ levelTime + 0.5)
           GameCombat.damage otherRef selfRef selfRef v3o (other^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

doorSecretUse :: EntUse
doorSecretUse =
  GenericEntUse "door_secret_use" $ \selfRef _ _ -> do
    self <- readEdictT selfRef
    v3o <- use $ globals.vec3Origin

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
    modifyEdictT selfRef (\v -> v & eNextThink .~ levelTime + 1.0
                                  & eThink .~ Just doorSecretMove2)
    return True

doorSecretMove2 :: EntThink
doorSecretMove2 =
  GenericEntThink "door_secret_move2" $ \selfRef -> do
    self <- readEdictT selfRef
    moveCalc selfRef (self^.ePos2) doorSecretMove3
    return True

doorSecretMove3 :: EntThink
doorSecretMove3 =
  GenericEntThink "door_secret_move3" $ \selfRef -> do
    self <- readEdictT selfRef

    if (self^.eWait) == -1
      then
        return True
      else do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        modifyEdictT selfRef (\v -> v & eNextThink .~ levelTime + (self^.eWait)
                                      & eThink .~ Just doorSecretMove4)
        return True

doorSecretMove4 :: EntThink
doorSecretMove4 =
  GenericEntThink "door_secret_move4" $ \selfRef -> do
    self <- readEdictT selfRef
    moveCalc selfRef (self^.ePos1) doorSecretMove5
    return True

doorSecretMove5 :: EntThink
doorSecretMove5 =
  GenericEntThink "door_secret_move5" $ \selfRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyEdictT selfRef (\v -> v & eNextThink .~ levelTime + 1.0
                                  & eThink .~ Just doorSecretMove6)
    return True

doorSecretMove6 :: EntThink
doorSecretMove6 =
  GenericEntThink "door_secret_move6" $ \selfRef -> do
    v3o <- use $ globals.vec3Origin
    moveCalc selfRef v3o doorSecretDone
    return True

doorSecretDone :: EntThink
doorSecretDone =
  GenericEntThink "door_secret_move7" $ \selfRef -> do
    self <- readEdictT selfRef

    when (isNothing (self^.eTargetName) || (self^.eSpawnFlags) .&. secretAlwaysShoot /= 0) $
      modifyEdictT selfRef (\v -> v & eHealth .~ 0
                                    & eTakeDamage .~ Constants.damageYes)

    doorUseAreaPortals selfRef False
    return True

doorSecretDie :: EntDie
doorSecretDie =
  GenericEntDie "door_secret_die" $ \selfRef _ attackerRef _ _ -> do
    modifyEdictT selfRef (\v -> v & eTakeDamage .~ Constants.damageNo)
    entUse doorSecretUse selfRef (Just attackerRef) (Just attackerRef)

spFuncDoorRotating :: EntThink
spFuncDoorRotating =
  GenericEntThink "sp_func_door_rotating" $ \edictRef -> do
    edict <- readEdictT edictRef
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

    modifyEdictT edictRef (\v -> v & eEntityState.esAngles .~ V3 0 0 0
                                   & eMoveDir .~ moveDir')
      
    use (gameBaseGlobals.gbSpawnTemp) >>= \spawnTemp ->
      when ((spawnTemp^.stDistance) == 0) $ do
        dprintf ((edict^.eClassName) `B.append` " at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` " with no distance set\n")
        gameBaseGlobals.gbSpawnTemp.stDistance .= 90

    spawnTemp <- use $ gameBaseGlobals.gbSpawnTemp

    let speed = if (edict^.eSpeed) == 0 then 100 else edict^.eSpeed

    modifyEdictT edictRef (\v -> v & ePos1 .~ V3 0 0 0
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
      modifyEdictT edictRef (\v -> v & eMoveInfo.miSoundStart .~ soundStart
                                     & eMoveInfo.miSoundMiddle .~ soundMiddle
                                     & eMoveInfo.miSoundEnd .~ soundEnd)

    -- if it starts open, switch the positions
    when ((edict^.eSpawnFlags) .&. Constants.doorStartOpen /= 0) $ do
      edict' <- readEdictT edictRef
      modifyEdictT edictRef (\v -> v & eEntityState.esAngles .~ (edict'^.ePos2)
                                     & ePos2 .~ (edict'^.ePos1)
                                     & ePos1 .~ (edict'^.ePos2)
                                     & eMoveDir %~ (fmap negate))

    when ((edict^.eHealth) /= 0) $
      modifyEdictT edictRef (\v -> v & eTakeDamage .~ Constants.damageYes
                                     & eDie .~ Just doorKilled
                                     & eMaxHealth .~ (edict^.eHealth))

    when (isJust (edict^.eTargetName) && isJust (edict^.eMessage)) $ do
      void $ soundIndex (Just "misc/talk.wav")
      modifyEdictT edictRef (\v -> v & eTouch .~ Just doorTouch)

    edict' <- readEdictT edictRef

    modifyEdictT edictRef (\v -> v & eMoveInfo.miState .~ stateBottom
                                   & eMoveInfo.miSpeed .~ (edict'^.eSpeed)
                                   & eMoveInfo.miAccel .~ (edict'^.eAccel)
                                   & eMoveInfo.miDecel .~ (edict'^.eDecel)
                                   & eMoveInfo.miWait .~ (edict'^.eWait)
                                   & eMoveInfo.miStartOrigin .~ (edict'^.eEntityState.esOrigin)
                                   & eMoveInfo.miStartAngles .~ (edict'^.ePos1)
                                   & eMoveInfo.miEndOrigin .~ (edict'^.eEntityState.esOrigin)
                                   & eMoveInfo.miEndAngles .~ (edict'^.ePos2))

    when ((edict'^.eSpawnFlags) .&. 16 /= 0) $
      modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAll))
      
    -- to simplify logic elsewhere, make non-teamed doors into a team of one
    when (isNothing (edict'^.eTeam)) $
      modifyEdictT edictRef (\v -> v & eTeamMaster .~ Just edictRef)

    linkEntity edictRef

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyEdictT edictRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime)

    let nextThink = if (edict'^.eHealth) /= 0 || isJust (edict'^.eTargetName)
                      then Just thinkCalcMoveSpeed
                      else Just thinkSpawnDoorTrigger

    modifyEdictT edictRef (\v -> v & eThink .~ nextThink)
    return True

spFuncConveyor :: EntThink
spFuncConveyor =
  GenericEntThink "sp_func_conveyor" $ \selfRef -> do
    self <- readEdictT selfRef

    let speed = if (self^.eSpeed) == 0 then 100 else self^.eSpeed
    modifyEdictT selfRef (\v -> v & eSpeed .~ speed)

    when ((self^.eSpawnFlags) .&. 1 == 0) $ do
      modifyEdictT selfRef (\v -> v & eCount .~ truncate speed
                                     & eSpeed .~ 0)

    modifyEdictT selfRef (\v -> v & eUse .~ Just funcConveyorUse)
    
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    setModel selfRef (self^.eiModel)
    modifyEdictT selfRef (\v -> v & eSolid .~ Constants.solidBsp)
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
    edict <- readEdictT edictRef
    setModel <- use $ gameBaseGlobals.gbGameImport.giSetModel

    setModel edictRef (edict^.eiModel)

    modifyEdictT edictRef (\v -> v & eUse .~ Just useKillBox
                                   & eSvFlags .~ Constants.svfNoClient)

    return True

spFuncRotating :: EntThink
spFuncRotating =
  GenericEntThink "sp_func_rotating" $ \edictRef -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidBsp)

    updateMoveType edictRef

    -- set the axis of rotation
    updateRotationAxis edictRef

    -- check for reverse rotation
    checkRevereseRotation edictRef

    checkSpeedAndDmg edictRef

    modifyEdictT edictRef (\v -> v & eUse .~ Just rotatingUse)

    edict <- readEdictT edictRef

    when ((edict^.eDmg) /= 0) $
      modifyEdictT edictRef (\v -> v & eBlocked .~ Just rotatingBlocked)

    when ((edict^.eSpawnFlags) .&. 1 /= 0) $
      entUse (fromJust $ edict^.eUse) edictRef Nothing Nothing

    when ((edict^.eSpawnFlags) .&. 64 /= 0) $
      modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAll))

    when ((edict^.eSpawnFlags) .&. 128 /= 0) $
      modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAllFast))

    setModel edictRef (edict^.eiModel)
    linkEntity edictRef

    return True

  where updateMoveType :: EdictReference -> Quake ()
        updateMoveType edictRef = do
          edict <- readEdictT edictRef
          let spawnFlags = edict^.eSpawnFlags

          let moveType = if spawnFlags .&. 32 /= 0
                           then Constants.moveTypeStop
                           else Constants.moveTypePush

          modifyEdictT edictRef (\v -> v & eMoveType .~ moveType)

        updateRotationAxis :: EdictReference -> Quake ()
        updateRotationAxis edictRef = do
          edict <- readEdictT edictRef
          let spawnFlags = edict^.eSpawnFlags

          let moveDir = if | spawnFlags .&. 4 /= 0 -> V3 0 0 1
                           | spawnFlags .&. 8 /= 0 -> V3 1 0 0
                           | otherwise -> V3 0 1 0

          modifyEdictT edictRef (\v -> v & eMoveDir .~ moveDir)

        checkRevereseRotation :: EdictReference -> Quake ()
        checkRevereseRotation edictRef = do
          edict <- readEdictT edictRef
          let spawnFlags = edict^.eSpawnFlags

          when (spawnFlags .&. 2 /= 0) $
            modifyEdictT edictRef (\v -> v & eMoveDir %~ (fmap (0 -)))

        checkSpeedAndDmg :: EdictReference -> Quake ()
        checkSpeedAndDmg edictRef = do
          edict <- readEdictT edictRef

          when ((edict^.eSpeed) == 0) $
            modifyEdictT edictRef (\v -> v & eSpeed .~ 100)

          when ((edict^.eDmg) == 0) $
            modifyEdictT edictRef (\v -> v & eDmg .~ 2)

triggerElevatorInit :: EntThink
triggerElevatorInit =
  GenericEntThink "trigger_elevator_init" $ \selfRef -> do
    self <- readEdictT selfRef
    dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf

    case self^.eTarget of
      Nothing -> do
        dprintf "trigger_elevator has no target\n"
        return True

      Just target -> do
        maybeMoveTargetRef <- GameBase.pickTarget (self^.eTarget)
        modifyEdictT selfRef (\v -> v & eMoveTarget .~ maybeMoveTargetRef)

        case maybeMoveTargetRef of
          Nothing -> do
            dprintf ("trigger_elevator unable to find target " `B.append` target `B.append` "\n")
            return True
            
          Just moveTargetRef -> do
            moveTarget <- readEdictT moveTargetRef

            if (moveTarget^.eClassName) /= "func_train"
              then do
                dprintf ("trigger_elevator target " `B.append` target `B.append` " is not a train\n")
                return True
              else do
                modifyEdictT selfRef (\v -> v & eUse .~ Just triggerElevatorUse
                                              & eSvFlags .~ Constants.svfNoClient)

                return True

spTriggerElevator :: EntThink
spTriggerElevator =
  GenericEntThink "sp_trigger_elevator" $ \selfRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT selfRef (\v -> v & eThink .~ Just triggerElevatorInit
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
spFuncPlat :: EdictReference -> Quake ()
spFuncPlat edictRef = do
    edict <- readEdictT edictRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity

    modifyEdictT edictRef (\v -> v & eEntityState.esAngles .~ V3 0 0 0
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

    modifyEdictT edictRef (\v -> v & ePos1 .~ pos1
                                   & ePos2 .~ pos2
                                   & eUse .~ Just usePlat)

    platSpawnInsideTrigger edictRef -- the "start moving" trigger

    case edict^.eTargetName of
      Just _ ->
        modifyEdictT edictRef (\v -> v & eMoveInfo.miState .~ stateUp)

      Nothing -> do
        modifyEdictT edictRef (\v -> v & eEntityState.esOrigin .~ pos2)
        linkEntity edictRef
        modifyEdictT edictRef (\v -> v & eMoveInfo.miState .~ stateBottom)

    edict' <- readEdictT edictRef

    soundStart <- soundIndex (Just "plats/pt1_strt.wav")
    soundMiddle <- soundIndex (Just "plats/pt1_mid.wav")
    soundEnd <- soundIndex (Just "plats/pt1_end.wav")

    modifyEdictT edictRef (\v -> v & eMoveInfo.miSpeed .~ (edict'^.eSpeed)
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
  GenericEntBlocked "plat_blocked" $ \_ _ -> do
    io (putStrLn "GameFunc.platBlocked") >> undefined -- TODO

usePlat :: EntUse
usePlat =
  GenericEntUse "use_plat" $ \_ _ _ -> do
    io (putStrLn "GameFunc.usePlat") >> undefined -- TODO

platSpawnInsideTrigger :: EdictReference -> Quake ()
platSpawnInsideTrigger _ = do
    io (putStrLn "GameFunc.platSpawnInsideTrigger") >> undefined -- TODO

spFuncWater :: EdictReference -> Quake ()
spFuncWater _ = io (putStrLn "GameFunc.spFuncWater") >> undefined -- TODO

spFuncTrain :: EdictReference -> Quake ()
spFuncTrain edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity
        dprintf = gameImport^.giDprintf

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                   & eEntityState.esAngles .~ V3 0 0 0
                                   & eBlocked .~ Just trainBlocked
                                   & eSolid .~ Constants.solidBsp)

    edict <- readEdictT edictRef

    if (edict^.eSpawnFlags) .&. trainBlockStops /= 0
      then
        modifyEdictT edictRef (\v -> v & eDmg .~ 0)

      else
        when ((edict^.eDmg) == 0) $
          modifyEdictT edictRef (\v -> v & eDmg .~ 100)

    setModel edictRef (edict^.eiModel)

    noise <- use $ gameBaseGlobals.gbSpawnTemp.stNoise

    when (isJust noise) $ do
      noiseIdx <- soundIndex noise
      modifyEdictT edictRef (\v -> v & eMoveInfo.miSoundMiddle .~ noiseIdx)

    let speed = if (edict^.eSpeed) == 0 then 100 else edict^.eSpeed
    modifyEdictT edictRef (\v -> v & eSpeed .~ speed
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
        modifyEdictT edictRef (\v -> v & eThink .~ Just funcTrainFind
                                       & eNextThink .~ levelTime + Constants.frameTime)

      else
        dprintf $ "func_train without a target at " `B.append` Lib.vtos (edict^.eAbsMin) `B.append` "\n"

spFuncTimer :: EdictReference -> Quake ()
spFuncTimer edictRef = do
    edict <- readEdictT edictRef

    when ((edict^.eWait) == 0) $
      modifyEdictT edictRef (\v -> v & eWait .~ 1)

    modifyEdictT edictRef (\v -> v & eUse .~ Just funcTimerUse
                                   & eThink .~ Just funcTimerThink)

    when ((edict^.eRandom) >= (edict^.eWait)) $ do
      modifyEdictT edictRef (\v -> v & eRandom .~ (edict^.eWait) - Constants.frameTime)
      dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
      dprintf $ "func_timer at " `B.append` Lib.vtos (edict^.eEntityState.esOrigin) `B.append` " has random >= wait\n"
    
    when (((edict^.eSpawnFlags) .&. 1) /= 0) $ do
      levelTime <- use $ gameBaseGlobals.gbLevel.llTime
      pauseTime <- use $ gameBaseGlobals.gbSpawnTemp.stPauseTime
      cr <- Lib.crandom

      modifyEdictT edictRef (\v -> v & eNextThink .~ levelTime + 1 + pauseTime + (edict^.eDelay) + (edict^.eWait) + cr * (edict^.eRandom)
                                     & eActivator .~ Just edictRef)

    modifyEdictT edictRef (\v -> v & eSvFlags .~ Constants.svfNoClient)

funcTimerUse :: EntUse
funcTimerUse =
  GenericEntUse "func_timer_use" $ \selfRef _ activator -> do
    modifyEdictT selfRef (\v -> v & eActivator .~ activator)

    -- if on, turn it off
    self <- readEdictT selfRef

    if (self^.eNextThink) /= 0
      then
        modifyEdictT selfRef (\v -> v & eNextThink .~ 0)
      else
        -- turn it on
        if (self^.eDelay) /= 0
          then do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime
            modifyEdictT selfRef (\v -> v & eNextThink .~ levelTime + (self^.eDelay))
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
    edict <- readEdictT edictRef

    GameUtil.useTargets edictRef (edict^.eActivator)

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    r <- Lib.crandom
    modifyEdictT edictRef (\v -> v & eNextThink .~ levelTime + (edict^.eWait) + r * (edict^.eRandom))

    return True

trainBlocked :: EntBlocked
trainBlocked =
  GenericEntBlocked "train_blocked" $ \_ _ -> do
    io (putStrLn "GameFunc.trainBlocked") >> undefined -- TODO

trainUse :: EntUse
trainUse =
  GenericEntUse "train_use" $ \selfRef _ activatorRef -> do
    modifyEdictT selfRef (\v -> v & eActivator .~ activatorRef)

    self <- readEdictT selfRef

    if (self^.eSpawnFlags) .&. trainStartOn /= 0
      then
        unless ((self^.eSpawnFlags) .&. trainToggle == 0) $ do
          modifyEdictT selfRef (\v -> v & eSpawnFlags %~ (.&. (complement trainStartOn))
                                        & eVelocity .~ V3 0 0 0
                                        & eNextThink .~ 0)

      else
        if isJust (self^.eTargetEnt)
          then trainResume selfRef
          else void $ think trainNext selfRef

funcTrainFind :: EntThink
funcTrainFind =
  GenericEntThink "func_train_find" $ \selfRef -> do
    self <- readEdictT selfRef
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
            ent <- readEdictT (fromJust entRef)

            let origin = ent^.eEntityState.esOrigin
                mins = self^.eMins

            modifyEdictT selfRef (\v -> v & eTarget .~ (ent^.eTarget)
                                          & eEntityState.esOrigin .~ (origin - mins))

            linkEntity selfRef

            -- if not triggered, start immediately
            when (isNothing (self^.eTargetName)) $
              modifyEdictT selfRef (\v -> v & eSpawnFlags %~ (.|. trainStartOn))

            updatedSelf <- readEdictT selfRef

            when ((updatedSelf^.eSpawnFlags) .&. trainStartOn /= 0) $ do
              levelTime <- use $ gameBaseGlobals.gbLevel.llTime

              modifyEdictT selfRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime
                                            & eThink .~ Just trainNext
                                            & eActivator .~ Just selfRef)

    return True

doorUse :: EntUse
doorUse =
  GenericEntUse "door_use" $ \selfRef _ activatorRef -> do
    self <- readEdictT selfRef

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
        triggerPairedDoors activatorRef (Just edictRef) = do
          edict <- readEdictT edictRef
          let teamChain = edict^.eTeamChain

          modifyEdictT edictRef (\v -> v & eMessage .~ Nothing
                                         & eTouch .~ Nothing)
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
  GenericEntThink "think_calc_movespeed" $ \edictRef -> do
    edict <- readEdictT edictRef

    -- only the team master does this
    unless ((edict^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
      -- find the smallest distance any member of the team will be moving
      minDist <- findSmallestDistance (edict^.eTeamChain) (abs $ edict^.eMoveInfo.miDistance)

      let time = minDist / (edict^.eMoveInfo.miSpeed)

      -- adjust speeds so they will all complete at the same time
      adjustSpeeds (Just edictRef) time

    return True

  where findSmallestDistance :: Maybe EdictReference -> Float -> Quake Float
        findSmallestDistance Nothing minDist = return minDist
        findSmallestDistance (Just entRef) minDist = do
          ent <- readEdictT entRef

          let dist = abs (ent^.eMoveInfo.miDistance)
              minDist' = if dist < minDist then dist else minDist

          findSmallestDistance (ent^.eTeamChain) minDist'

        adjustSpeeds :: Maybe EdictReference -> Float -> Quake ()
        adjustSpeeds Nothing _ = return ()
        adjustSpeeds (Just entRef) time = do
          ent <- readEdictT entRef

          let newspeed = (abs $ ent^.eMoveInfo.miSpeed) / time
              ratio = newspeed / (ent^.eMoveInfo.miSpeed)
              accel = if (ent^.eMoveInfo.miAccel) == (ent^.eMoveInfo.miSpeed)
                        then newspeed
                        else (ent^.eMoveInfo.miAccel) * ratio
              decel = if (ent^.eMoveInfo.miDecel) == (ent^.eMoveInfo.miSpeed)
                        then newspeed
                        else (ent^.eMoveInfo.miDecel) * ratio

          modifyEdictT entRef (\v -> v & eMoveInfo.miAccel .~ accel
                                       & eMoveInfo.miDecel .~ decel
                                       & eMoveInfo.miSpeed .~ newspeed)

          adjustSpeeds (ent^.eTeamChain) time

thinkSpawnDoorTrigger :: EntThink
thinkSpawnDoorTrigger =
  GenericEntThink "think_spawn_door_trigger" $ \edictRef -> do
    edict <- readEdictT edictRef

    -- only the team leader spawns a trigger
    unless ((edict^.eFlags) .&. Constants.flTeamSlave /= 0) $ do
      let mins = edict^.eMins
          maxs = edict^.eMaxs

      (mins', maxs') <- teamChainAddPointToBound (edict^.eTeamChain) mins maxs

      let expandedMins = (V3 (-60) (-60) 0) + mins'
          expandedMaxs = (V3 60 60 0) + maxs'

      otherRef <- GameUtil.spawn

      modifyEdictT otherRef (\v -> v & eMins .~ expandedMins
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

  where teamChainAddPointToBound :: Maybe EdictReference -> V3 Float -> V3 Float -> Quake (V3 Float, V3 Float)
        teamChainAddPointToBound Nothing mins maxs = return (mins, maxs)
        teamChainAddPointToBound (Just otherRef) mins maxs = do
          other <- readEdictT otherRef

          let (mins', maxs') = GameBase.addPointToBound (other^.eAbsMin) mins maxs
              (mins'', maxs'') = GameBase.addPointToBound (other^.eAbsMax) mins' maxs'

          teamChainAddPointToBound (other^.eTeamChain) mins'' maxs''

buttonUse :: EntUse
buttonUse =
  GenericEntUse "button_use" $ \selfRef _ activatorRef -> do
    modifyEdictT selfRef (\v -> v & eActivator .~ activatorRef)
    void $ think buttonFire selfRef

buttonTouch :: EntTouch
buttonTouch =
  GenericEntTouch "button_touch" $ \_ _ _ _ -> do
    io (putStrLn "GameFunc.buttonTouch") >> undefined -- TODO

buttonKilled :: EntDie
buttonKilled =
  GenericEntDie "button_killed" $ \selfRef _ attackerRef _ _ -> do
    self <- readEdictT selfRef

    modifyEdictT selfRef (\v -> v & eActivator .~ Just attackerRef
                                  & eHealth .~ (self^.eMaxHealth)
                                  & eTakeDamage .~ Constants.damageNo)

    void $ think buttonFire selfRef

buttonFire :: EntThink
buttonFire =
  GenericEntThink "button_fire" $ \_ -> do
    io (putStrLn "GameFunc.buttonFire") >> undefined -- TODO

trainNext :: EntThink
trainNext =
  GenericEntThink "train_next" $ \edictRef -> do
    (done, entRef) <- pickNextTarget edictRef True

    unless done $ do
      edict <- readEdictT edictRef
      ent <- readEdictT (fromJust entRef)

      modifyEdictT edictRef (\v -> v & eMoveInfo.miWait .~ (ent^.eMoveInfo.miWait)
                                     & eTargetEnt .~ entRef)

      when ((edict^.eFlags) .&. Constants.flTeamSlave == 0) $ do
        when ((edict^.eMoveInfo.miSoundStart) /= 0) $ do
          sound <- use $ gameBaseGlobals.gbGameImport.giSound
          sound (Just edictRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (edict^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0

        modifyEdictT edictRef (\v -> v & eEntityState.esSound .~ (edict^.eMoveInfo.miSoundMiddle))

      let dest = (ent^.eEntityState.esOrigin) - (edict^.eMins)

      modifyEdictT edictRef (\v -> v & eMoveInfo.miState .~ Constants.stateTop
                                     & eMoveInfo.miStartOrigin .~ (edict^.eEntityState.esOrigin)
                                     & eMoveInfo.miEndOrigin .~ dest)

      moveCalc edictRef dest trainWait

      modifyEdictT edictRef (\v -> v & eSpawnFlags %~ (.|. trainStartOn))

    return True

  where pickNextTarget :: EdictReference -> Bool -> Quake (Bool, Maybe EdictReference)
        pickNextTarget selfRef first = do
          self <- readEdictT selfRef
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
                  ent <- readEdictT (fromJust entRef)
                  modifyEdictT selfRef (\v -> v & eTarget .~ (ent^.eTarget))

                  -- check for a teleport path_corner
                  if (ent^.eSpawnFlags) .&. 1 /= 0
                    then
                      if not first
                        then do
                          dprintf $ "connected teleport path_corner, see " `B.append` (ent^.eClassName) `B.append` " at " `B.append` (Lib.vtos (ent^.eEntityState.esOrigin)) `B.append` "\n"
                          return (True, entRef)
                        else do
                          let origin = ((ent^.eEntityState.esOrigin) - (self^.eMins))

                          modifyEdictT selfRef (\v -> v & eEntityState.esOrigin .~ origin
                                                        & eEntityState.esOldOrigin .~ origin
                                                        & eEntityState.esEvent .~ Constants.evOtherTeleport)

                          linkEntity selfRef

                          pickNextTarget selfRef False

                    else return (False, entRef)

touchDoorTrigger :: EntTouch
touchDoorTrigger =
  GenericEntTouch "touch_door_trigger" $ \selfRef otherRef _ _ -> do
    self <- readEdictT selfRef
    other <- readEdictT otherRef

    let Just ownerRef = self^.eOwner
    owner <- readEdictT ownerRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let skip = (other^.eHealth) <= 0 
            || ((other^.eSvFlags) .&. Constants.svfMonster == 0 && isNothing (other^.eClient))
            || ((owner^.eSpawnFlags) .&. Constants.doorNoMonster /= 0 && (other^.eSvFlags) .&. Constants.svfMonster /= 0)
            || levelTime < (self^.eTouchDebounceTime)

    unless skip $ do
      modifyEdictT selfRef (\v -> v & eTouchDebounceTime .~ levelTime + 1)
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
doorUseAreaPortals selfRef open = do
    self <- readEdictT selfRef

    case self^.eTarget of
      Nothing -> return ()
      Just target -> setAreaPortals target Nothing

  where setAreaPortals :: B.ByteString -> Maybe EdictReference -> Quake ()
        setAreaPortals target ref = do
          maybeFoundRef <- GameBase.gFind ref GameBase.findByTarget target

          case maybeFoundRef of
            Nothing -> return ()
            Just foundRef -> do
              foundEdict <- readEdictT foundRef
              when (BC.map toLower (foundEdict^.eClassName) == "func_areaportal") $ do
                setAreaPortalState <- use $ gameBaseGlobals.gbGameImport.giSetAreaPortalState
                setAreaPortalState (foundEdict^.eStyle) open
              setAreaPortals target maybeFoundRef

trainResume :: EdictReference -> Quake ()
trainResume _ = io (putStrLn "GameFunc.trainResume") >> undefined -- TODO

trainWait :: EntThink
trainWait =
  GenericEntThink "train_wait" $ \edictRef -> do
    done <- checkPathTarget edictRef

    unless done $ do
      edict <- readEdictT edictRef

      if edict^.eMoveInfo.miWait /= 0
        then do
          if | edict^.eMoveInfo.miWait > 0 -> do
                 levelTime <- use $ gameBaseGlobals.gbLevel.llTime

                 modifyEdictT edictRef (\v -> v & eNextThink .~ levelTime + (edict^.eMoveInfo.miWait)
                                                & eThink .~ Just trainNext)

             | (edict^.eSpawnFlags) .&. trainToggle /= 0 -> do
                 void $ think trainNext edictRef

                 modifyEdictT edictRef (\v -> v & eSpawnFlags %~ (.&. (complement trainStartOn))
                                                & eVelocity .~ V3 0 0 0
                                                & eNextThink .~ 0)

             | otherwise -> return ()

          when ((edict^.eFlags) .&. Constants.flTeamSlave == 0) $ do
            when ((edict^.eMoveInfo.miSoundEnd) /= 0) $ do
              sound <- use $ gameBaseGlobals.gbGameImport.giSound
              sound (Just edictRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (edict^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0

            modifyEdictT edictRef (\v -> v & eEntityState.esSound .~ 0)

        else void $ think trainNext edictRef

    return True

  where checkPathTarget :: EdictReference -> Quake Bool
        checkPathTarget edictRef = do
          edict <- readEdictT edictRef
          let Just targetRef = edict^.eTargetEnt
          target <- readEdictT targetRef

          if isJust $ target^.ePathTarget
            then do
              let saveTarget = target^.eTarget
              modifyEdictT targetRef (\v -> v & eTarget .~ (target^.ePathTarget))

              GameUtil.useTargets targetRef (edict^.eActivator)

              modifyEdictT targetRef (\v -> v & eTarget .~ saveTarget)

              -- make sure we didn't get killed by a killtarget
              edict' <- readEdictT edictRef
              let inUse = edict'^.eInUse
              return $ if inUse then False else True

            else return False

moveCalc :: EdictReference -> V3 Float -> EntThink -> Quake ()
moveCalc edictRef dest func = do
    modifyEdictT edictRef (\v -> v & eVelocity .~ V3 0 0 0)

    edict <- readEdictT edictRef

    let dir = dest - (edict^.eEntityState.esOrigin)

    modifyEdictT edictRef (\v -> v & eMoveInfo.miDir .~ normalize dir
                                   & eMoveInfo.miRemainingDistance .~ sqrt (quadrance dir) -- TODO: make sure we are correct here
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
            modifyEdictT edictRef (\v -> v & eNextThink .~ levelTime + Constants.frameTime
                                           & eThink .~ Just moveBegin)

      else do
        -- aceelerative
        modifyEdictT edictRef (\v -> v & eMoveInfo.miCurrentSpeed .~ 0
                                       & eThink .~ Just thinkAccelMove
                                       & eNextThink .~ levelTime + Constants.frameTime)

angleMoveCalc :: EdictReference -> EntThink -> Quake ()
angleMoveCalc _ _ = do
    io (putStrLn "GameFunc.angleMoveCalc") >> undefined -- TODO

moveBegin :: EntThink
moveBegin =
  GenericEntThink "move_begin" $ \edictRef -> do
    edict <- readEdictT edictRef
    let moveInfo = edict^.eMoveInfo

    if (moveInfo^.miSpeed) * Constants.frameTime >= (moveInfo^.miRemainingDistance)
      then do
        void $ think moveFinal edictRef
        return True

      else do
        let velocity = fmap (* (moveInfo^.miSpeed)) (moveInfo^.miDir)
            frames :: Int = floor $ ((moveInfo^.miRemainingDistance) / (moveInfo^.miSpeed)) / Constants.frameTime
            framesF :: Float = fromIntegral frames

        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyEdictT edictRef (\v -> v & eVelocity .~ velocity
                                       & eMoveInfo.miRemainingDistance -~ framesF * (moveInfo^.miSpeed) * Constants.frameTime
                                       & eNextThink .~ levelTime + (framesF * Constants.frameTime)
                                       & eThink .~ Just moveFinal)

        return True

thinkAccelMove :: EntThink
thinkAccelMove =
  GenericEntThink "think_accelmove" $ \_ -> do
    io (putStrLn "GameFunc.thinkAccelMove") >> undefined -- TODO

moveFinal :: EntThink
moveFinal =
  GenericEntThink "move_final" $ \edictRef -> do
    edict <- readEdictT edictRef

    if (edict^.eMoveInfo.miRemainingDistance) == 0
      then do
        void $ think moveDone edictRef
        return True
      else do
        let velocity = fmap (* ((edict^.eMoveInfo.miRemainingDistance) / Constants.frameTime)) (edict^.eMoveInfo.miDir)
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime
        
        modifyEdictT edictRef (\v -> v & eVelocity .~ velocity
                                       & eThink .~ Just moveDone
                                       & eNextThink .~ levelTime + Constants.frameTime)

        return True

moveDone :: EntThink
moveDone =
  GenericEntThink "move_done" $ \edictRef -> do
    modifyEdictT edictRef (\v -> v & eVelocity .~ V3 0 0 0)
    edict <- readEdictT edictRef
    let endFunc = edict^.eMoveInfo.miEndFunc

    void $ think (fromJust endFunc) edictRef

    return True

rotatingUse :: EntUse
rotatingUse =
  GenericEntUse "rotating_use" $ \selfRef _ _ -> do
    self <- readEdictT selfRef
    vec3origin <- use $ globals.vec3Origin

    if (self^.eAVelocity) /= vec3origin
      then
        modifyEdictT selfRef (\v -> v & eEntityState.esSound .~ 0
                                      & eAVelocity .~ V3 0 0 0
                                      & eTouch .~ Nothing)

      else do
        modifyEdictT selfRef (\v -> v & eEntityState.esSound .~ (self^.eMoveInfo.miSoundMiddle)
                                      & eAVelocity .~ fmap (* (self^.eSpeed)) (self^.eMoveDir))

        when ((self^.eSpawnFlags) .&. 16 /= 0) $
          modifyEdictT selfRef (\v -> v & eTouch .~ Just rotatingTouch)

rotatingBlocked :: EntBlocked
rotatingBlocked =
  GenericEntBlocked "rotating_blocked" $ \_ _ -> do
    io (putStrLn "GameFunc.rotatingBlocked") >> undefined -- TODO

rotatingTouch :: EntTouch
rotatingTouch =
  GenericEntTouch "rotating_touch" $ \_ _ _ _ -> do
    io (putStrLn "GameFunc.rotatingTouch") >> undefined -- TODO

doorGoUp :: EdictReference -> Maybe EdictReference -> Quake ()
doorGoUp selfRef activatorRef = do
    self <- readEdictT selfRef

    unless ((self^.eMoveInfo.miState) == Constants.stateUp) $ do
      if (self^.eMoveInfo.miState) == Constants.stateTop
        then do
          -- reset top wait time
          when ((self^.eMoveInfo.miWait) >= 0) $ do
            levelTime <- use $ gameBaseGlobals.gbLevel.llTime
            modifyEdictT selfRef (\v -> v & eNextThink .~ levelTime + (self^.eMoveInfo.miWait))

        else do
          when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
            when ((self^.eMoveInfo.miSoundStart) /= 0) $ do
              sound <- use $ gameBaseGlobals.gbGameImport.giSound
              sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0

            modifyEdictT selfRef (\v -> v & eEntityState.esSound .~ self^.eMoveInfo.miSoundMiddle)

          modifyEdictT selfRef (\v -> v & eMoveInfo.miState .~ Constants.stateUp)

          if | (self^.eClassName) == "func_door" -> moveCalc selfRef (self^.eMoveInfo.miEndOrigin) doorHitTop
             | (self^.eClassName) == "func_door_rotating" -> angleMoveCalc selfRef doorHitTop
             | otherwise -> return ()

          GameUtil.useTargets selfRef activatorRef
          doorUseAreaPortals selfRef True

doorHitTop :: EntThink
doorHitTop =
  GenericEntThink "door_hit_top" $ \selfRef -> do
    self <- readEdictT selfRef

    when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((self^.eMoveInfo.miSoundEnd) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0

      modifyEdictT selfRef (\v -> v & eEntityState.esSound .~ 0)

    modifyEdictT selfRef (\v -> v & eMoveInfo.miState .~ Constants.stateTop)

    if (self^.eSpawnFlags) .&. Constants.doorToggle /= 0
      then
        return True

      else do
        when ((self^.eMoveInfo.miWait) >= 0) $ do
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime

          modifyEdictT selfRef (\v -> v & eThink .~ Just doorGoDown
                                        & eNextThink .~ levelTime + (self^.eMoveInfo.miWait))

        return True

doorGoDown :: EntThink
doorGoDown =
  GenericEntThink "door_go_down" $ \selfRef -> do
    self <- readEdictT selfRef

    when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((self^.eMoveInfo.miSoundStart) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundStart) 1 Constants.attnStatic 0

      modifyEdictT selfRef (\v -> v & eEntityState.esSound .~ self^.eMoveInfo.miSoundMiddle)

    when ((self^.eMaxHealth) /= 0) $ do
      modifyEdictT selfRef (\v -> v & eTakeDamage .~ Constants.damageYes
                                    & eHealth .~ self^.eMaxHealth)

    modifyEdictT selfRef (\v -> v & eMoveInfo.miState .~ Constants.stateDown)

    if | (self^.eClassName) == "func_door" -> moveCalc selfRef (self^.eMoveInfo.miStartOrigin) doorHitBottom
       | (self^.eClassName) == "func_door_rotating" -> angleMoveCalc selfRef doorHitBottom
       | otherwise -> return ()

    return True

doorHitBottom :: EntThink
doorHitBottom =
  GenericEntThink "door_hit_bottom" $ \selfRef -> do
    self <- readEdictT selfRef

    when ((self^.eFlags) .&. Constants.flTeamSlave == 0) $ do
      when ((self^.eMoveInfo.miSoundEnd) /= 0) $ do
        sound <- use $ gameBaseGlobals.gbGameImport.giSound
        sound (Just selfRef) (Constants.chanNoPhsAdd + Constants.chanVoice) (self^.eMoveInfo.miSoundEnd) 1 Constants.attnStatic 0
      
      modifyEdictT selfRef (\v -> v & eEntityState.esSound .~ 0)

    modifyEdictT selfRef (\v -> v & eMoveInfo.miState .~ Constants.stateBottom)
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
    self <- readEdictT selfRef

    if (self^.eSpawnFlags) .&. 1 /= 0
      then do
        modifyEdictT selfRef (\v -> v & eSpeed .~ 0
                                      & eSpawnFlags %~ (.&. (complement 1)))
      else do
        modifyEdictT selfRef (\v -> v & eSpeed .~ fromIntegral (self^.eCount)
                                      & eSpawnFlags %~ (.|. 1))

    self' <- readEdictT selfRef
    let spawnFlags = self'^.eSpawnFlags
    when (spawnFlags .&. 2 == 0) $
      modifyEdictT selfRef (\v -> v & eCount .~ 0)

triggerElevatorUse :: EntUse
triggerElevatorUse =
  GenericEntUse "trigger_elevator_use" $ \_ _ _ -> do
    io (putStrLn "GameFunc.triggerElevatorUse") >> undefined -- TODO
