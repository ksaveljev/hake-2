{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameMisc where

import Control.Lens (use, preuse, (^.), ix, (.=), zoom, (%=), (&), (+~), (+=), (-=), (.~), (%~), (-~))
import Control.Monad (liftM, when, void, unless)
import Data.Bits ((.|.), (.&.), complement, xor)
import Data.Char (ord)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3(..), _x, _y, _z, normalize)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Client.M as M
import {-# SOURCE #-} qualified Game.GameBase as GameBase
import qualified Game.GameCombat as GameCombat
import qualified Game.GameFunc as GameFunc
import qualified Game.GameUtil as GameUtil
import qualified Util.Lib as Lib
import qualified Util.Math3D as Math3D

{-
- QUAKED light (0 1 0) (-8 -8 -8) (8 8 8) START_OFF Non-displayed light.
- Default light value is 300. Default style is 0. If targeted, will toggle
- between on and off. Default _cone value is 10 (used to set size of light
- for spotlights)
-}
startOff :: Int
startOff = 1

lightUse :: EntUse
lightUse =
  GenericEntUse "light_use" $ \selfRef _ _ -> do
    self <- readEdictT selfRef
    configString <- use $ gameBaseGlobals.gbGameImport.giConfigString

    if (self^.eSpawnFlags) .&. startOff /= 0
      then do
        configString (Constants.csLights + (self^.eStyle)) "m"
        modifyEdictT selfRef (\v -> v & eSpawnFlags %~ (.&. (complement startOff)))
      else do
        configString (Constants.csLights + (self^.eStyle)) "a"
        modifyEdictT selfRef (\v -> v & eSpawnFlags %~ (.|. startOff))

spPathCorner :: EdictReference -> Quake ()
spPathCorner edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let dprintf = gameImport^.giDprintf
        linkEntity = gameImport^.giLinkEntity

    edict <- readEdictT edictRef

    case edict^.eTargetName of
      Nothing -> do
        dprintf $ "path_corner with no targetname at " `B.append` BC.pack (show (edict^.eEntityState.esOrigin)) `B.append` "\n"
        GameUtil.freeEdict edictRef

      Just _ -> do
        modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidTrigger
                                       & eTouch .~ Just pathCornerTouch
                                       & eMins .~ V3 (-8) (-8) (-8)
                                       & eMaxs .~ V3 8 8 8
                                       & eSvFlags %~ (.|. Constants.svfNoClient))

        linkEntity edictRef

{-
- QUAKED path_corner (.5 .3 0) (-8 -8 -8) (8 8 8) TELEPORT Target: next
- path corner Pathtarget: gets used when an entity that has this
- path_corner targeted touches it
-}
pathCornerTouch :: EntTouch
pathCornerTouch =
  GenericEntTouch "path_corner_touch" $ \selfRef otherRef _ _ -> do
    done <- shouldReturn selfRef otherRef

    unless done $ do
      saveTarget selfRef otherRef
      target <- pickTarget selfRef

      nextTarget <- if isJust target
                      then pickNextTarget (fromJust target) otherRef
                      else return Nothing

      modifyEdictT otherRef (\v -> v & eGoalEntity .~ nextTarget
                                     & eMoveTarget .~ nextTarget)

      self <- readEdictT selfRef
      let wait = self^.eWait

      if wait /= 0
        then do
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime
          other <- readEdictT otherRef
          let stand = other^.eMonsterInfo.miStand
          modifyEdictT otherRef (\v -> v & eMonsterInfo.miPauseTime .~ levelTime + wait)
          void $ think (fromJust stand) otherRef

        else do
          other <- readEdictT otherRef

          case other^.eMoveTarget of
            Nothing -> do
              levelTime <- use $ gameBaseGlobals.gbLevel.llTime
              modifyEdictT otherRef (\v -> v & eMonsterInfo.miPauseTime .~ levelTime + 100000000)
              void $ think (fromJust $ other^.eMonsterInfo.miStand) otherRef

            Just _ -> do
              let Just goalRef = other^.eGoalEntity
              goal <- readEdictT goalRef
              let a = (goal^.eEntityState.esOrigin) - (other^.eEntityState.esOrigin)
              modifyEdictT otherRef (\v -> v & eIdealYaw .~ Math3D.vectorYaw a)

  where shouldReturn :: EdictReference -> EdictReference -> Quake Bool
        shouldReturn selfRef otherRef = do
          other <- readEdictT otherRef

          let moveTarget = other^.eMoveTarget
              enemyRef = other^.eEnemy

          if moveTarget /= (Just selfRef) || isJust enemyRef
            then return True
            else return False

        saveTarget :: EdictReference -> EdictReference -> Quake ()
        saveTarget selfRef otherRef = do
          self <- readEdictT selfRef
          let target = self^.eTarget
          
          modifyEdictT selfRef (\v -> v & eTarget .~ self^.ePathTarget)
          GameUtil.useTargets selfRef (Just otherRef)
          modifyEdictT selfRef (\v -> v & eTarget .~ target)

        pickTarget :: EdictReference -> Quake (Maybe EdictReference)
        pickTarget selfRef = do
          self <- readEdictT selfRef
          let target = self^.eTarget
          case target of
            Nothing -> return Nothing
            Just _ -> GameBase.pickTarget target

        pickNextTarget :: EdictReference -> EdictReference -> Quake (Maybe EdictReference)
        pickNextTarget edictRef otherRef = do
          edict <- readEdictT edictRef

          if (edict^.eSpawnFlags) .&. 1 /= 0
            then do
              other <- readEdictT otherRef

              let v = edict^.eEntityState.esOrigin
                  -- v[2] += next.mins[2];
                  -- v[2] -= other.mins[2];
                  v' = v & _z +~ ((edict^.eMins._z) - (other^.eMins._z))

              modifyEdictT otherRef (\v -> v & eEntityState.esOrigin .~ v')
              next <- GameBase.pickTarget (edict^.eTarget)
              modifyEdictT otherRef (\v -> v & eEntityState.esEvent .~ Constants.evOtherTeleport)
              return next

            else
              return (Just edictRef)

spPointCombat :: EdictReference -> Quake ()
spPointCombat edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then
        GameUtil.freeEdict edictRef

      else do
        modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidTrigger
                                       & eTouch .~ Just pointCombatTouch
                                       & eMins .~ V3 (-8) (-8) (-16)
                                       & eMaxs .~ V3 8 8 16
                                       & eSvFlags .~ Constants.svfNoClient)

        linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
        linkEntity edictRef

spViewThing :: EdictReference -> Quake ()
spViewThing edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let dprintf = gameImport^.giDprintf
        linkEntity = gameImport^.giLinkEntity
        modelIndex = gameImport^.giModelIndex

    modelIdx <- modelIndex (Just "models/objects/banner/tris.md2")

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                   & eSolid .~ Constants.solidBbox
                                   & eEntityState.esRenderFx .~ Constants.rfFrameLerp
                                   & eMins .~ V3 (-16) (-16) (-24)
                                   & eMaxs .~ V3 16 16 32
                                   & eEntityState.esModelIndex .~ modelIdx)

    linkEntity edictRef

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT edictRef (\v -> v & eNextThink .~ levelTime + 0.5
                                   & eThink .~ Just thViewThing)

{-
- QUAKED viewthing (0 .5 .8) (-8 -8 -8) (8 8 8) Just for the debugging
- level. Don't use
-}
thViewThing :: EntThink
thViewThing =
  GenericEntThink "th_viewthing" $ \edictRef -> do
    edict <- readEdictT edictRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT edictRef (\v -> v & eEntityState.esFrame .~ ((edict^.eEntityState.esFrame) + 1) `mod` 7
                                   & eNextThink .~ levelTime + Constants.frameTime)

    return True

{-
- QUAKED info_null (0 0.5 0) (-4 -4 -4) (4 4 4) Used as a positional target
- for spotlights, etc.
-}
spInfoNull :: EdictReference -> Quake ()
spInfoNull = GameUtil.freeEdict

{-
- QUAKED info_notnull (0 0.5 0) (-4 -4 -4) (4 4 4) Used as a positional
- target for lightning.
-}
spInfoNotNull :: EdictReference -> Quake ()
spInfoNotNull edictRef = do
    edict <- readEdictT edictRef

    modifyEdictT edictRef (\v -> v & eAbsMin .~ (edict^.eEntityState.esOrigin)
                                   & eAbsMax .~ (edict^.eEntityState.esOrigin))

spLight :: EdictReference -> Quake ()
spLight edictRef = do
    -- no targeted lights in deathmatch, because they cause global messages
    edict <- readEdictT edictRef
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if isNothing (edict^.eTargetName) || deathmatchValue /= 0
      then
        GameUtil.freeEdict edictRef

      else 
        when ((edict^.eStyle) >= 32) $ do
          modifyEdictT edictRef (\v -> v & eUse .~ Just lightUse)

          configString <- use $ gameBaseGlobals.gbGameImport.giConfigString

          if (edict^.eSpawnFlags) .&. startOff /= 0
            then configString (Constants.csLights + (edict^.eStyle)) "a"
            else configString (Constants.csLights + (edict^.eStyle)) "m"

spFuncWall :: EdictReference -> Quake ()
spFuncWall edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let linkEntity = gameImport^.giLinkEntity
        setModel = gameImport^.giSetModel

    readEdictT edictRef >>= \edict ->
      setModel edictRef (edict^.eiModel)

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypePush)

    updateEdictEffects

    isAWall <- checkWall

    if isAWall
      then do
        -- just a wall
        modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidBsp)
        linkEntity edictRef

      else do
        -- it must be TRIGGER_SPAWN
        checkTriggerSpawn

        -- yell if the spawnflags are odd
        checkOddSpawnFlags

        modifyEdictT edictRef (\v -> v & eUse .~ Just funcWallUse)

        edict <- readEdictT edictRef
        let spawnFlags = edict^.eSpawnFlags

        if spawnFlags .&. 4 /= 0
          then
            modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidBsp)
          else
            modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidNot
                                           & eSvFlags %~ (.|. Constants.svfNoClient))

        linkEntity edictRef

  where updateEdictEffects :: Quake ()
        updateEdictEffects = do
          edict <- readEdictT edictRef
          let spawnFlags = edict^.eSpawnFlags

          when (spawnFlags .&. 8 /= 0) $
            modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAll))

          when (spawnFlags .&. 16 /= 0) $
            modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAllFast))

        checkWall :: Quake Bool
        checkWall = do
          edict <- readEdictT edictRef
          return $ (edict^.eSpawnFlags) .&. 7 == 0

        checkTriggerSpawn :: Quake ()
        checkTriggerSpawn = do
          edict <- readEdictT edictRef
          let spawnFlags = edict^.eSpawnFlags

          when (spawnFlags .&. 1 == 0) $ do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf "func_wall missing TRIGGER_SPAWN\n"
            modifyEdictT edictRef (\v -> v & eSpawnFlags %~ (.|. 1))

        checkOddSpawnFlags :: Quake ()
        checkOddSpawnFlags = do
          edict <- readEdictT edictRef
          let spawnFlags = edict^.eSpawnFlags

          when (spawnFlags .&. 4 /= 0 && spawnFlags .&. 2 == 0) $ do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf "func_wall START_ON without TOGGLE\n"
            modifyEdictT edictRef (\v -> v & eSpawnFlags %~ (.|. 2))

spFuncObject :: EdictReference -> Quake ()
spFuncObject selfRef = do
    self <- readEdictT selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

        mins = fmap (+ 1) (self^.eMins)
        maxs = fmap (subtract 1) (self^.eMaxs)
        dmg = if (self^.eDmg) == 0 then 100 else self^.eDmg
        effects = if (self^.eSpawnFlags) .&. 2 /= 0 then (self^.eEntityState.esEffects) .|. Constants.efAnimAll else self^.eEntityState.esEffects
        effects' = if (self^.eSpawnFlags) .&. 4 /= 0 then (self^.eEntityState.esEffects) .|. Constants.efAnimAllFast else effects

    if (self^.eSpawnFlags) == 0
      then do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyEdictT selfRef (\v -> v & eMins .~ mins
                                      & eMaxs .~ maxs
                                      & eDmg .~ dmg
                                      & eEntityState.esEffects .~ effects'
                                      & eClipMask .~ Constants.maskMonsterSolid
                                      & eSolid .~ Constants.solidBsp
                                      & eMoveType .~ Constants.moveTypePush
                                      & eThink .~ Just funcObjectRelease
                                      & eNextThink .~ levelTime + 2 * Constants.frameTime)

      else
        modifyEdictT selfRef (\v -> v & eMins .~ mins
                                      & eMaxs .~ maxs
                                      & eDmg .~ dmg
                                      & eEntityState.esEffects .~ effects'
                                      & eClipMask .~ Constants.maskMonsterSolid
                                      & eSolid .~ Constants.solidNot
                                      & eMoveType .~ Constants.moveTypePush
                                      & eUse .~ Just funcObjectUse
                                      & eSvFlags %~ (.|. Constants.svfNoClient))

    linkEntity selfRef

spFuncExplosive :: EdictReference -> Quake ()
spFuncExplosive edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then
        GameUtil.freeEdict edictRef

      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity
            setModel = gameImport^.giSetModel

        modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypePush)

        void $ modelIndex (Just "models/objects/debris1/tris.md2")
        void $ modelIndex (Just "models/objects/debris2/tris.md2")

        edict <- readEdictT edictRef

        setModel edictRef (edict^.eiModel)

        if (edict^.eSpawnFlags) .&. 1 /= 0
          then
            modifyEdictT edictRef (\v -> v & eSvFlags %~ (.|. Constants.svfNoClient)
                                           & eSolid .~ Constants.solidNot
                                           & eUse .~ Just funcExplosiveSpawn)

          else do
            modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidBsp)
            when (isJust (edict^.eTargetName)) $
              modifyEdictT edictRef (\v -> v & eUse .~ Just funcExplosiveUse)

        when ((edict^.eSpawnFlags) .&. 2 /= 0) $
          modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAll))

        when ((edict^.eSpawnFlags) .&. 4 /= 0) $
          modifyEdictT edictRef (\v -> v & eEntityState.esEffects %~ (.|. Constants.efAnimAllFast))

        edict' <- readEdictT edictRef
        case edict'^.eUse of
          Just (FuncExplosiveUse _ _) -> return ()
          _ -> do
            when ((edict^.eHealth) == 0) $
              modifyEdictT edictRef (\v -> v & eHealth .~ 100)

            modifyEdictT edictRef (\v -> v & eDie .~ Just funcExplosiveExplode
                                           & eTakeDamage .~ Constants.damageYes)

        linkEntity edictRef

spMiscExploBox :: EdictReference -> Quake ()
spMiscExploBox edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then
        GameUtil.freeEdict edictRef

      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        void $ modelIndex (Just "models/objects/debris1/tris.md2")
        void $ modelIndex (Just "models/objects/debris2/tris.md2")
        void $ modelIndex (Just "models/objects/debris3/tris.md2")

        let trisModel = "models/objects/barrels/tris.md2"
        tris <- modelIndex (Just trisModel)

        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidBbox
                                       & eMoveType .~ Constants.moveTypeStep
                                       & eiModel .~ Just trisModel
                                       & eEntityState.esModelIndex .~ tris
                                       & eMins .~ V3 (-16) (-16) 0
                                       & eMaxs .~ V3 16 16 40
                                       & eDie .~ Just barrelDelay
                                       & eTakeDamage .~ Constants.damageYes
                                       & eMonsterInfo.miAIFlags .~ Constants.aiNoStep
                                       & eTouch .~ Just barrelTouch
                                       & eThink .~ Just M.dropToFloor
                                       & eNextThink .~ levelTime + 2 * Constants.frameTime)

        edict <- readEdictT edictRef

        when ((edict^.eMass) == 0) $
          modifyEdictT edictRef (\v -> v & eMass .~ 400)

        when ((edict^.eHealth) == 0) $
          modifyEdictT edictRef (\v -> v & eHealth .~ 10)

        when ((edict^.eDmg) == 0) $
          modifyEdictT edictRef (\v -> v & eDmg .~ 150)

        linkEntity edictRef

spMiscBlackHole :: EdictReference -> Quake ()
spMiscBlackHole edictRef = do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/objects/black/tris.md2")

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                   & eSolid .~ Constants.solidNot
                                   & eMins .~ V3 (-64) (-64) 0
                                   & eMaxs .~ V3 64 64 8
                                   & eEntityState.esModelIndex .~ modelIdx
                                   & eEntityState.esRenderFx .~ Constants.rfTranslucent
                                   & eUse .~ Just miscBlackHoleUse
                                   & eThink .~ Just miscBlackHoleThink
                                   & eNextThink .~ levelTime + 2 * Constants.frameTime)

    linkEntity edictRef

spMiscEasterTank :: EdictReference -> Quake ()
spMiscEasterTank edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/monsters/tank/tris.md2")

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                   & eSolid .~ Constants.solidBbox
                                   & eMins .~ V3 (-32) (-32) (-16)
                                   & eMaxs .~ V3 32 32 32
                                   & eEntityState.esModelIndex .~ modelIdx
                                   & eEntityState.esFrame .~ 254
                                   & eThink .~ Just miscEasterTankThink
                                   & eNextThink .~ levelTime + 2 * Constants.frameTime)

    linkEntity edictRef

spMiscEasterChick :: EdictReference -> Quake ()
spMiscEasterChick edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIndex <- modelIndex (Just "models/monsters/bitch/tris.md2")

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                   & eSolid .~ Constants.solidBbox
                                   & eMins .~ V3 (-32) (-32) 0
                                   & eMaxs .~ V3 32 32 32
                                   & eEntityState.esModelIndex .~ modelIndex
                                   & eEntityState.esFrame .~ 208
                                   & eThink .~ Just miscEasterChickThink
                                   & eNextThink .~ levelTime + 2 * Constants.frameTime)

    linkEntity edictRef

spMiscEasterChick2 :: EdictReference -> Quake ()
spMiscEasterChick2 edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/monsters/bitch/tris.md2")

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                   & eSolid .~ Constants.solidBbox
                                   & eMins .~ V3 (-32) (-32) 0
                                   & eMaxs .~ V3 32 32 32
                                   & eEntityState.esModelIndex .~ modelIdx
                                   & eEntityState.esFrame .~ 248
                                   & eThink .~ Just miscEasterChick2Think
                                   & eNextThink .~ levelTime + 2 * Constants.frameTime)

    linkEntity edictRef

spMonsterCommanderBody :: EdictReference -> Quake ()
spMonsterCommanderBody selfRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity
        soundIndex = gameImport^.giSoundIndex

    modelIdx <- modelIndex (Just "models/monsters/commandr/tris.md2")

    modifyEdictT selfRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                  & eSolid .~ Constants.solidBbox
                                  & eiModel .~ Just "models/monsters/commandr/tris.md2"
                                  & eEntityState.esModelIndex .~ modelIdx
                                  & eMins .~ V3 (-32) (-32) 0
                                  & eMaxs .~ V3 32 32 48
                                  & eUse .~ Just commanderBodyUse
                                  & eTakeDamage .~ Constants.damageYes
                                  & eFlags .~ Constants.flGodMode
                                  & eEntityState.esRenderFx %~ (.|. Constants.rfFrameLerp))

    linkEntity selfRef
    
    void $ soundIndex (Just "tank/thud.wav")
    void $ soundIndex (Just "tank/pain.wav")

    modifyEdictT selfRef (\v -> v & eThink .~ Just commanderBodyDrop
                                  & eNextThink .~ levelTime + 5 * Constants.frameTime)

spMiscBanner :: EdictReference -> Quake ()
spMiscBanner edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/objects/banner/tris.md2")
    r <- Lib.rand
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                   & eSolid .~ Constants.solidNot
                                   & eEntityState.esModelIndex .~ modelIdx
                                   & eEntityState.esFrame .~ (fromIntegral r) `mod` 16)

    linkEntity edictRef

    modifyEdictT edictRef (\v -> v & eThink .~ Just miscBannerThink
                                   & eNextThink .~ levelTime + Constants.frameTime)

spMiscDeadSoldier :: EdictReference -> Quake ()
spMiscDeadSoldier edictRef = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then
        GameUtil.freeEdict edictRef

      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        modelIdx <- modelIndex (Just "models/deadbods/dude/tris.md2")

        modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                       & eSolid .~ Constants.solidBbox
                                       & eEntityState.esModelIndex .~ modelIdx)

        edict <- readEdictT edictRef
        let spawnFlags = edict^.eSpawnFlags

        -- defaults to frame 0
        let frame = if | spawnFlags .&. 2 /= 0 -> 1
                       | spawnFlags .&. 4 /= 0 -> 2
                       | spawnFlags .&. 8 /= 0 -> 3
                       | spawnFlags .&. 16 /= 0 -> 4
                       | spawnFlags .&. 32 /= 0 -> 5
                       | otherwise -> 0

        modifyEdictT edictRef (\v -> v & eEntityState.esFrame .~ frame
                                       & eMins .~ V3 (-16) (-16) 0
                                       & eMaxs .~ V3 16 16 16
                                       & eDeadFlag .~ Constants.deadDead
                                       & eTakeDamage .~ Constants.damageYes
                                       & eSvFlags %~ (.|. (Constants.svfMonster .|. Constants.svfDeadMonster))
                                       & eDie .~ Just miscDeadSoldierDie
                                       & eMonsterInfo.miAIFlags %~ (.|. Constants.aiGoodGuy))

        linkEntity edictRef

spMiscViper :: EdictReference -> Quake ()
spMiscViper edictRef = do
    edict <- readEdictT edictRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let dprintf = gameImport^.giDprintf
        modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    case edict^.eTarget of
      Nothing -> do
        dprintf ("misc_viper without a target at " `B.append` Lib.vtos (edict^.eAbsMin) `B.append` "\n")
        GameUtil.freeEdict edictRef

      Just _ -> do
        let speed = if (edict^.eSpeed) == 0 then 300 else edict^.eSpeed

        modelIdx <- modelIndex (Just "models/ships/viper/tris.md2")
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyEdictT edictRef (\v -> v & eSpeed .~ speed
                                       & eMoveType .~ Constants.moveTypePush
                                       & eSolid .~ Constants.solidNot
                                       & eEntityState.esModelIndex .~ modelIdx
                                       & eMins .~ V3 (-16) (-16) 0
                                       & eMaxs .~ V3 16 16 32
                                       & eThink .~ Just GameFunc.funcTrainFind
                                       & eNextThink .~ levelTime + Constants.frameTime
                                       & eUse .~ Just miscViperUse
                                       & eSvFlags %~ (.|. Constants.svfNoClient)
                                       & eMoveInfo.miAccel .~ speed
                                       & eMoveInfo.miDecel .~ speed
                                       & eMoveInfo.miSpeed .~ speed)

        linkEntity edictRef

miscViperUse :: EntUse
miscViperUse =
  GenericEntUse "misc_viper_use" $ \selfRef otherRef activatoRef -> do
    modifyEdictT selfRef (\v -> v & eSvFlags %~ (.&. (complement Constants.svfNoClient))
                                  & eUse .~ Just GameFunc.trainUse)

    entUse (GameFunc.trainUse) selfRef otherRef activatoRef

{-
- QUAKED misc_bigviper (1 .5 0) (-176 -120 -24) (176 120 72) This is a
- large stationary viper as seen in Paul's intro
-}
spMiscBigViper :: EdictReference -> Quake ()
spMiscBigViper edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/ships/bigviper/tris.md2")

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                   & eSolid .~ Constants.solidBbox
                                   & eMins .~ V3 (-176) (-120) (-24)
                                   & eMaxs .~ V3 176 120 72
                                   & eEntityState.esModelIndex .~ modelIdx)

    linkEntity edictRef

spMiscViperBomb :: EdictReference -> Quake ()
spMiscViperBomb selfRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/objects/bomb/tris.md2")

    modifyEdictT selfRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                  & eSolid .~ Constants.solidNot
                                  & eMins .~ V3 (-8) (-8) (-8)
                                  & eMaxs .~ V3 8 8 8
                                  & eEntityState.esModelIndex .~ modelIdx
                                  & eDmg %~ (\v -> if v == 0 then 1000 else v)
                                  & eUse .~ Just miscViperBombUse
                                  & eSvFlags %~ (.|. Constants.svfNoClient))

    linkEntity selfRef

miscViperBombUse :: EntUse
miscViperBombUse =
  GenericEntUse "misc_viper_bomb_use" $ \selfRef _ activatorRef -> do
    modifyEdictT selfRef (\v -> v & eSolid .~ Constants.solidBbox
                                  & eSvFlags %~ (.&. (complement Constants.svfNoClient))
                                  & eEntityState.esEffects %~ (.|. Constants.efRocket)
                                  & eUse .~ Nothing
                                  & eMoveType .~ Constants.moveTypeToss
                                  & ePrethink .~ Just miscViperBombPrethink
                                  & eTouch .~ Just miscViperBombTouch
                                  & eActivator .~ activatorRef)

    es <- GameBase.gFind Nothing GameBase.findByClass "misc_viper"

    case es of
      Nothing ->
        return () -- jake2 doesn't do it though

      Just viperRef -> do
        viper <- readEdictT viperRef
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyEdictT selfRef (\v -> v & eVelocity .~ fmap (* (viper^.eMoveInfo.miSpeed)) (viper^.eMoveInfo.miDir)
                                      & eTimeStamp .~ levelTime
                                      & eMoveInfo.miDir .~ (viper^.eMoveInfo.miDir))

miscViperBombPrethink :: EntThink
miscViperBombPrethink =
  GenericEntThink "misc_viper_bomb_prethink" $ \selfRef -> do
    self <- readEdictT selfRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let diff = (self^.eTimeStamp) - levelTime
        diff' = if diff < (-1.0) then -1.0 else diff
        V3 a b c = fmap (* (1.0 + diff')) (self^.eMoveInfo.miDir)
        V3 a' b' c' = Math3D.vectorAngles (V3 a b diff')

    modifyEdictT selfRef (\v -> v & eGroundEntity .~ Nothing
                                  & eEntityState.esAngles .~ V3 a' b' ((self^.eEntityState.esAngles._z) + 10))

    return True

{-
- QUAKED misc_viper_bomb (1 0 0) (-8 -8 -8) (8 8 8) "dmg" how much boom
- should the bomb make?
-}
miscViperBombTouch :: EntTouch
miscViperBombTouch =
  GenericEntTouch "misc_viper_bomb_touch" $ \selfRef _ _ _ -> do
    self <- readEdictT selfRef

    GameUtil.useTargets selfRef (self^.eActivator)

    modifyEdictT selfRef (\v -> v & eEntityState.esOrigin._z .~ (self^.eAbsMin._z) + 1)

    GameCombat.radiusDamage selfRef selfRef (fromIntegral $ self^.eDmg) Nothing (fromIntegral (self^.eDmg) + 40) Constants.modBomb
    becomeExplosion2 selfRef

spMiscStroggShip :: EdictReference -> Quake ()
spMiscStroggShip edictRef = do
    edict <- readEdictT edictRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let dprintf = gameImport^.giDprintf
        modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    case (edict^.eTarget) of
      Nothing -> do
        dprintf $ (edict^.eClassName) `B.append` " without a target at " `B.append`
                  Lib.vtos (edict^.eAbsMin) `B.append` "\n"
        GameUtil.freeEdict edictRef

      Just _ -> do
        modelIdx <- modelIndex (Just "models/ships/strogg1/tris.md2")
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                       & eSolid .~ Constants.solidNot
                                       & eEntityState.esModelIndex .~ modelIdx
                                       & eMins .~ V3 (-16) (-16) 0
                                       & eMaxs .~ V3 16 16 32
                                       & eThink .~ Just (GameFunc.funcTrainFind)
                                       & eNextThink .~ levelTime + Constants.frameTime
                                       & eUse .~ Just (miscStroggShipUse)
                                       & eSvFlags %~ (.|. Constants.svfNoClient)
                                       & eMoveInfo.miAccel .~ (edict^.eSpeed)
                                       & eMoveInfo.miDecel .~ (edict^.eSpeed)
                                       & eMoveInfo.miSpeed .~ (edict^.eSpeed))

        linkEntity edictRef

spMiscSatelliteDish :: EdictReference -> Quake ()
spMiscSatelliteDish edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/objects/satellite/tris.md2")

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                   & eSolid .~ Constants.solidBbox
                                   & eMins .~ V3 (-64) (-64) 0
                                   & eMaxs .~ V3 64 64 128
                                   & eEntityState.esModelIndex .~ modelIdx
                                   & eUse .~ Just miscSatelliteDishUse)

    linkEntity edictRef

miscSatelliteDishUse :: EntUse
miscSatelliteDishUse =
  GenericEntUse "misc_satellite_dish_use" $ \selfRef _ _ -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT selfRef (\v -> v & eEntityState.esFrame .~ 0
                                  & eThink .~ Just miscSatelliteDishThink
                                  & eNextThink .~ levelTime + Constants.frameTime)

{-
- QUAKED misc_satellite_dish (1 .5 0) (-64 -64 0) (64 64 128)
-}
miscSatelliteDishThink :: EntThink
miscSatelliteDishThink =
  GenericEntThink "misc_satellite_dish_think" $ \selfRef -> do
    self <- readEdictT selfRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    if (self^.eEntityState.esFrame) + 1 < 38
      then
        modifyEdictT selfRef (\v -> v & eEntityState.esFrame +~ 1
                                      & eNextThink .~ levelTime + Constants.frameTime)
      else
        modifyEdictT selfRef (\v -> v & eEntityState.esFrame +~ 1)

    return True

{-
- QUAKED light_mine1 (0 1 0) (-2 -2 -12) (2 2 12)
-}
spLightMine1 :: EdictReference -> Quake ()
spLightMine1 edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/objects/minelite/light1/tris.md2")

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                   & eSolid .~ Constants.solidBbox
                                   & eEntityState.esModelIndex .~ modelIdx)

    linkEntity edictRef

{-
- QUAKED light_mine2 (0 1 0) (-2 -2 -12) (2 2 12)
-}
spLightMine2 :: EdictReference -> Quake ()
spLightMine2 edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/objects/minelite/light2/tris.md2")

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypeNone
                                   & eSolid .~ Constants.solidBbox
                                   & eEntityState.esModelIndex .~ modelIdx)
    
    linkEntity edictRef

{-
- QUAKED misc_gib_arm (1 0 0) (-8 -8 -8) (8 8 8) Intended for use with the
- target_spawner
-}
spMiscGibArm :: EdictReference -> Quake ()
spMiscGibArm edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    r1 <- Lib.randomF
    r2 <- Lib.randomF
    r3 <- Lib.randomF

    modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidNot
                                   & eEntityState.esEffects %~ (.|. Constants.efGib)
                                   & eTakeDamage .~ Constants.damageYes
                                   & eDie .~ Just gibDie
                                   & eMoveType .~ Constants.moveTypeToss
                                   & eSvFlags %~ (.|. Constants.svfMonster)
                                   & eDeadFlag .~ Constants.deadDead
                                   & eAVelocity .~ V3 (r1 * 200) (r2 * 200) (r3 * 200)
                                   & eThink .~ Just GameUtil.freeEdictA
                                   & eNextThink .~ levelTime + 30)

    setModel edictRef (Just "models/objects/gibs/arm/tris.md2")

    linkEntity edictRef

{-
- QUAKED misc_gib_leg (1 0 0) (-8 -8 -8) (8 8 8) Intended for use with the
- target_spawner
-}
spMiscGibLeg :: EdictReference -> Quake ()
spMiscGibLeg edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    r1 <- Lib.randomF
    r2 <- Lib.randomF
    r3 <- Lib.randomF

    modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidNot
                                   & eEntityState.esEffects %~ (.|. Constants.efGib)
                                   & eTakeDamage .~ Constants.damageYes
                                   & eDie .~ Just gibDie
                                   & eMoveType .~ Constants.moveTypeToss
                                   & eSvFlags %~ (.|. Constants.svfMonster)
                                   & eDeadFlag .~ Constants.deadDead
                                   & eAVelocity .~ V3 (r1 * 200) (r2 * 200) (r3 * 200)
                                   & eThink .~ Just GameUtil.freeEdictA
                                   & eNextThink .~ levelTime + 30)

    setModel edictRef (Just "models/objects/gibs/leg/tris.md2")

    linkEntity edictRef

{-
- QUAKED misc_gib_head (1 0 0) (-8 -8 -8) (8 8 8) Intended for use with the
- target_spawner
-}
spMiscGibHead :: EdictReference -> Quake ()
spMiscGibHead edictRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    r1 <- Lib.randomF
    r2 <- Lib.randomF
    r3 <- Lib.randomF

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    
    modifyEdictT edictRef (\v -> v & eSolid .~ Constants.solidNot
                                   & eEntityState.esEffects %~ (.|. Constants.efGib)
                                   & eTakeDamage .~ Constants.damageYes
                                   & eDie .~ Just gibDie
                                   & eMoveType .~ Constants.moveTypeToss
                                   & eSvFlags %~ (.|. Constants.svfMonster)
                                   & eDeadFlag .~ Constants.deadDead
                                   & eAVelocity .~ V3 (r1 * 200) (r2 * 200) (r3 * 200)
                                   & eThink .~ Just GameUtil.freeEdictA
                                   & eNextThink .~ levelTime + 30)

    setModel edictRef (Just "models/objects/gibs/head/tris.md2")

    linkEntity edictRef

{-
- QUAKED target_character (0 0 1) ? used with target_string (must be on
- same "team") "count" is position in the string (starts at 1)
-}
spTargetCharacter :: EdictReference -> Quake ()
spTargetCharacter selfRef = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    self <- readEdictT selfRef

    modifyEdictT selfRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                  & eSolid .~ Constants.solidBsp
                                  & eEntityState.esFrame .~ 12)

    setModel selfRef (self^.eiModel)

    linkEntity selfRef

spTargetString :: EdictReference -> Quake ()
spTargetString selfRef = do
    modifyEdictT selfRef (\v -> v & eMessage %~ (\v -> if isNothing v then Just "" else v)
                                  & eUse .~ Just targetStringUse)

{-
- QUAKED target_string (0 0 1) (-8 -8 -8) (8 8 8)
-}
targetStringUse :: EntUse
targetStringUse =
  GenericEntUse "target_string_use" $ \selfRef _ _ -> do
    self <- readEdictT selfRef
    let Just message = self^.eMessage
    setFrame message (self^.eTeamMaster)

  where setFrame :: B.ByteString -> Maybe EdictReference -> Quake ()
        setFrame _ Nothing = return ()
        setFrame message (Just edictRef) = do
          edict <- readEdictT edictRef

          if (edict^.eCount) == 0
            then
              setFrame message (edict^.eTeamChain)

            else do
              let n = (edict^.eCount) - 1

              if n >= B.length message
                then do
                  modifyEdictT edictRef (\v -> v & eEntityState.esFrame .~ 12)
                  setFrame message (edict^.eTeamChain)

                else do
                  let c = message `BC.index` n
                      frame = if | c >= '0' && c <= '9' -> ord c - ord '0'
                                 | c == '-' -> 10
                                 | c == ':' -> 11
                                 | otherwise -> 12

                  modifyEdictT edictRef (\v -> v & eEntityState.esFrame .~ frame)
                  setFrame message (edict^.eTeamChain)

spFuncClock :: EdictReference -> Quake ()
spFuncClock selfRef = do
    self <- readEdictT selfRef

    if | isNothing (self^.eTarget) -> do
           dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
           dprintf ((self^.eClassName) `B.append` " with no target at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` "\n")
           GameUtil.freeEdict selfRef

       | (self^.eSpawnFlags) .&. 2 /= 0 && (self^.eCount) == 0 -> do
           dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
           dprintf ((self^.eClassName) `B.append` " with no count at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` "\n")
           GameUtil.freeEdict selfRef

       | otherwise -> do
           when ((self^.eSpawnFlags) .&. 1 /= 0 && (self^.eCount) == 0) $
             modifyEdictT selfRef (\v -> v & eCount .~ 60 * 60)

           funcClockReset selfRef

           modifyEdictT selfRef (\v -> v & eMessage .~ Just ""
                                         & eThink .~ Just funcClockThink)

           self' <- readEdictT selfRef

           if (self'^.eSpawnFlags) .&. 4 /= 0
             then 
               modifyEdictT selfRef (\v -> v & eUse .~ Just funcClockUse)
             else do
               levelTime <- use $ gameBaseGlobals.gbLevel.llTime
               modifyEdictT selfRef (\v -> v & eNextThink .~ levelTime + 1)

-- don't let field width of any clock messages change, or it
-- could cause an overwrite after a game load
funcClockReset :: EdictReference -> Quake ()
funcClockReset selfRef = do
    self <- readEdictT selfRef

    if (self^.eSpawnFlags) .&. 1 /= 0
      then
        modifyEdictT selfRef (\v -> v & eActivator .~ Nothing
                                      & eHealth .~ 0
                                      & eWait .~ fromIntegral (self^.eCount))
      else
        modifyEdictT selfRef (\v -> v & eActivator .~ Nothing
                                      & eHealth .~ (self^.eCount)
                                      & eWait .~ 0)

{-
- QUAKED func_clock (0 0 1) (-8 -8 -8) (8 8 8) TIMER_UP TIMER_DOWN
- START_OFF MULTI_USE target a target_string with this
- 
- The default is to be a time of day clock
- 
- TIMER_UP and TIMER_DOWN run for "count" seconds and the fire "pathtarget"
- If START_OFF, this entity must be used before it starts
- 
- "style" 0 "xx" 1 "xx:xx" 2 "xx:xx:xx"
-}
funcClockThink :: EntThink
funcClockThink =
  GenericEntThink "func_clock_think" $ \selfRef -> do
    done <- checkEnemy selfRef

    if done
      then
        return True

      else do
        self <- readEdictT selfRef

        if | (self^.eSpawnFlags) .&. 1 /= 0 -> do
               funcClockFormatCountdown selfRef
               modifyEdictT selfRef (\v -> v & eHealth +~ 1)

           | (self^.eSpawnFlags) .&. 2 /= 0 -> do
               funcClockFormatCountdown selfRef
               modifyEdictT selfRef (\v -> v & eHealth -~ 1)

           | otherwise -> do
               modifyEdictT selfRef (\v -> v & eMessage .~ Just "SOME TIME") -- TODO: FIXME: show time

        io (putStrLn "GameMisc.funcClockThink") >> undefined -- TODO

  where checkEnemy :: EdictReference -> Quake Bool
        checkEnemy selfRef = do
          self <- readEdictT selfRef

          case self^.eEnemy of
            Nothing -> do
              es <- GameBase.gFind Nothing GameBase.findByTarget (fromJust $ self^.eTarget)

              case es of
                Nothing ->
                  return True

                Just _ -> do
                  modifyEdictT selfRef (\v -> v & eEnemy .~ es)
                  return False

            _ -> return False

funcClockFormatCountdown :: EdictReference -> Quake ()
funcClockFormatCountdown selfRef = do
    self <- readEdictT selfRef

    if | (self^.eStyle) == 0 ->
           modifyEdictT selfRef (\v -> v & eMessage .~ Just (BC.pack $ show (self^.eHealth))) -- IMPROVE?

       | (self^.eStyle) == 1 ->
           modifyEdictT selfRef (\v -> v & eMessage .~ Just (BC.pack (show ((self^.eHealth) `div` 60)) `B.append` ":" `B.append` BC.pack (show ((self^.eHealth) `mod` 60))))

       | (self^.eStyle) == 2 ->
           io (putStrLn "GameMisc.funcClockFormatCountdown") >> undefined -- TODO

       | otherwise ->
           return ()

funcClockUse :: EntUse
funcClockUse =
  GenericEntUse "func_clock_use" $ \selfRef _ activatorRef -> do
    self <- readEdictT selfRef

    when ((self^.eSpawnFlags) .&. 8 == 0) $
      modifyEdictT selfRef (\v -> v & eUse .~ Nothing)

    when (isNothing (self^.eActivator)) $ do
      modifyEdictT selfRef (\v -> v & eActivator .~ activatorRef)
      void $ think (fromJust $ self^.eThink) selfRef

{-
- QUAKED misc_teleporter (1 0 0) (-32 -32 -24) (32 32 -16) Stepping onto
- this disc will teleport players to the targeted misc_teleporter_dest
- object.
-}
spMiscTeleporter :: EdictReference -> Quake ()
spMiscTeleporter edictRef = do
    edict <- readEdictT edictRef
    gameImport <- use $ gameBaseGlobals.gbGameImport
    
    let dprintf = gameImport^.giDprintf
        setModel = gameImport^.giSetModel
        soundIndex = gameImport^.giSoundIndex
        linkEntity = gameImport^.giLinkEntity

    case edict^.eTarget of
      Nothing -> do
        dprintf "teleporter without a target.\n"
        GameUtil.freeEdict edictRef

      Just _ -> do
        setModel edictRef (Just "models/objects/dmspot/tris.md2")
        soundIdx <- soundIndex (Just "world/amb10.wav")

        modifyEdictT edictRef (\v -> v & eEntityState.esSkinNum .~ 1
                                       & eEntityState.esEffects .~ Constants.efTeleporter
                                       & eEntityState.esSound .~ soundIdx
                                       & eSolid .~ Constants.solidBbox
                                       & eMins .~ V3 (-32) (-32) (-24)
                                       & eMaxs .~ V3 32 32 (-16))

        linkEntity edictRef

        trigRef <- GameUtil.spawn
        edict' <- readEdictT edictRef

        modifyEdictT trigRef (\v -> v & eTouch .~ Just teleporterTouch
                                      & eSolid .~ Constants.solidTrigger
                                      & eTarget .~ (edict'^.eTarget)
                                      & eOwner .~ Just edictRef
                                      & eEntityState.esOrigin .~ (edict'^.eEntityState.esOrigin)
                                      & eMins .~ V3 (-8) (-8) 8
                                      & eMaxs .~ V3 8 8 24)

        linkEntity trigRef

teleporterTouch :: EntTouch
teleporterTouch =
  GenericEntTouch "teleporter_touch" $ \_ _ _ _ -> do
    io (putStrLn "GameMisc.teleporterTouch") >> undefined -- TODO

{-
- QUAKED func_areaportal (0 0 0) ?
- 
- This is a non-visible object that divides the world into areas that are
- seperated when this portal is not activated. Usually enclosed in the
- middle of a door.
-}
spFuncAreaPortal :: EntThink
spFuncAreaPortal =
  GenericEntThink "sp_func_areaportal" $ \edictRef -> do
    modifyEdictT edictRef (\v -> v & eUse .~ Just useAreaPortal
                                   & eCount .~ 0) -- always start closed

    return True

{-
- QUAKED misc_teleporter_dest (1 0 0) (-32 -32 -24) (32 32 -16) Point
- teleporters at these.
-}
spMiscTeleporterDest :: EntThink
spMiscTeleporterDest =
  GenericEntThink "SP_misc_teleporter_dest" $ \edictRef -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    setModel edictRef (Just "models/objects/dmspot/tris.md2")

    modifyEdictT edictRef (\v -> v & eEntityState.esSkinNum .~ 0
                                   & eSolid .~ Constants.solidBbox
                                   & eMins .~ V3 (-32) (-32) (-24)
                                   & eMaxs .~ V3 32 32 (-16))

    linkEntity edictRef
    return True

{-
- QUAKED misc_deadsoldier (1 .5 0) (-16 -16 0) (16 16 16) ON_BACK
- ON_STOMACH BACK_DECAP FETAL_POS SIT_DECAP IMPALED This is the dead player
- model. Comes in 6 exciting different poses!
-}
miscDeadSoldierDie :: EntDie
miscDeadSoldierDie =
  GenericEntDie "misc_deadsoldier_die" $ \selfRef _ _ damage _ -> do
    self <- readEdictT selfRef

    unless ((self^.eHealth) > (-80)) $ do
      gameImport <- use $ gameBaseGlobals.gbGameImport

      let sound = gameImport^.giSound
          soundIndex = gameImport^.giSoundIndex

      soundIdx <- soundIndex (Just "misc/udeath.wav")
      sound (Just selfRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0

      throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
      throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
      throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic
      throwGib selfRef "models/objects/gibs/sm_meat/tris.md2" damage Constants.gibOrganic

      throwHead selfRef "models/objects/gibs/head2/tris.md2" damage Constants.gibOrganic

throwGib :: EdictReference -> B.ByteString -> Int -> Int -> Quake ()
throwGib selfRef gibName damage gibType = do
    self <- readEdictT selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    gibRef <- GameUtil.spawn

    let size = fmap (* 0.5) (self^.eSize)
        origin = (self^.eAbsMin) + size

    r1 <- Lib.crandom
    r2 <- Lib.crandom
    r3 <- Lib.crandom

    let a = (origin^._x) + r1 * (size^._x)
        b = (origin^._y) + r2 * (size^._y)
        c = (origin^._z) + r3 * (size^._z)

    setModel gibRef (Just gibName)
    
    modifyEdictT gibRef (\v -> v & eEntityState.esOrigin .~ V3 a b c
                                 & eSolid .~ Constants.solidNot
                                 & eEntityState.esEffects %~ (.|. Constants.efGib)
                                 & eFlags %~ (.|. Constants.flNoKnockback)
                                 & eTakeDamage .~ Constants.damageYes
                                 & eDie .~ Just gibDie)

    vscale <- if gibType == Constants.gibOrganic
                then do
                  modifyEdictT gibRef (\v -> v & eMoveType .~ Constants.moveTypeToss
                                               & eTouch .~ Just gibTouch)

                  return 0.5

                else do
                  modifyEdictT gibRef (\v -> v & eMoveType .~ Constants.moveTypeBounce)
                  return 1.0

    vd <- velocityForDamage damage
    modifyEdictT gibRef (\v -> v & eVelocity .~ (self^.eVelocity) + fmap (* vscale) vd)

    clipGibVelocity gibRef

    r1' <- Lib.randomF
    r2' <- Lib.randomF
    r3' <- Lib.randomF

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    r <- Lib.randomF

    modifyEdictT gibRef (\v -> v & eAVelocity .~ V3 (r1' * 600) (r2' * 600) (r3' * 600)
                                 & eThink .~ Just GameUtil.freeEdictA
                                 & eNextThink .~ levelTime + 10 + r * 10)

    linkEntity gibRef

gibTouch :: EntTouch
gibTouch =
  GenericEntTouch "gib_touch" $ \selfRef _ plane _ -> do
    self <- readEdictT selfRef

    case self^.eGroundEntity of
      Nothing ->
        return ()

      Just _ -> do
        modifyEdictT selfRef (\v -> v & eTouch .~ Nothing)

        -- TODO: jake2 checks if plane != null
        gameImport <- use $ gameBaseGlobals.gbGameImport

        let sound = gameImport^.giSound
            soundIndex = gameImport^.giSoundIndex

        soundIdx <- soundIndex (Just "misc/fhit3.wav")
        sound (Just selfRef) Constants.chanVoice soundIdx 1 Constants.attnNorm 0

        let normalAngles = Math3D.vectorAngles (plane^.cpNormal)
            (_, Just right, _) = Math3D.angleVectors normalAngles False True False

        modifyEdictT selfRef (\v -> v & eEntityState.esAngles .~ Math3D.vectorAngles right)

        smMeatIndex <- use $ gameBaseGlobals.gbSmMeatIndex

        when ((self^.eEntityState.esModelIndex) == smMeatIndex) $ do
          levelTime <- use $ gameBaseGlobals.gbLevel.llTime

          modifyEdictT selfRef (\v -> v & eEntityState.esFrame +~ 1
                                        & eThink .~ Just gibThink
                                        & eNextThink .~ levelTime + Constants.frameTime)

gibThink :: EntThink
gibThink =
  GenericEntThink "gib_think" $ \selfRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT selfRef (\v -> v & eEntityState.esFrame +~ 1
                                  & eNextThink .~ levelTime + Constants.frameTime)

    self <- readEdictT selfRef

    when ((self^.eEntityState.esFrame) == 10) $ do
      r <- Lib.randomF

      modifyEdictT selfRef (\v -> v & eThink .~ Just GameUtil.freeEdictA
                                    & eNextThink .~ levelTime + 8 + r * 10)

    return True

clipGibVelocity :: EdictReference -> Quake ()
clipGibVelocity edictRef = do
    edict <- readEdictT edictRef

    let V3 a b c = edict^.eVelocity
        a' = if | a < -300 -> -300
                | a > 300 -> 300
                | otherwise -> a
        b' = if | b < -300 -> -300
                | b > 300 -> 300
                | otherwise -> b
        c' = if | c < 200 -> 200 -- always some upwards
                | c > 500 -> 500
                | otherwise -> c

    modifyEdictT edictRef (\v -> v & eVelocity .~ V3 a' b' c')

{-
- QUAKED func_group (0 0 0) ? Used to group brushes together just for
- editor convenience.
-}
velocityForDamage :: Int -> Quake (V3 Float)
velocityForDamage damage = do
    r1 <- Lib.crandom
    r2 <- Lib.crandom
    r3 <- Lib.crandom

    let v = V3 (100 * r1) (100 * r2) (200 + 100 * r3)

    return $ if damage < 50
               then fmap (* 0.7) v
               else fmap (* 1.2) v

throwHead :: EdictReference -> B.ByteString -> Int -> Int -> Quake ()
throwHead selfRef gibName damage gibType = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    modifyEdictT selfRef (\v -> v & eEntityState.esSkinNum .~ 0
                                  & eEntityState.esFrame .~ 0
                                  & eMins .~ V3 0 0 0
                                  & eMaxs .~ V3 0 0 0
                                  & eEntityState.esModelIndex2 .~ 0
                                  & eSolid .~ Constants.solidNot
                                  & eEntityState.esEffects %~ (.|. Constants.efGib)
                                  & eEntityState.esEffects %~ (.&. (complement Constants.efFlies))
                                  & eEntityState.esSound .~ 0
                                  & eFlags %~ (.|. Constants.flNoKnockback)
                                  & eSvFlags %~ (.&. (complement Constants.svfMonster))
                                  & eTakeDamage .~ Constants.damageYes
                                  & eDie .~ Just gibDie)

    setModel selfRef (Just gibName)

    vscale <- if gibType == Constants.gibOrganic
                then do
                  modifyEdictT selfRef (\v -> v & eMoveType .~ Constants.moveTypeToss
                                                & eTouch .~ Just gibTouch)

                  return 0.5
                else do
                  modifyEdictT selfRef (\v -> v & eMoveType .~ Constants.moveTypeBounce)
                  return 1.0

    vd <- velocityForDamage damage
    modifyEdictT selfRef (\v -> v & eVelocity +~ fmap (* vscale) vd)
    clipGibVelocity selfRef

    r <- Lib.crandom
    r' <- Lib.randomF
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT selfRef (\v -> v & eAVelocity._y .~ r * 600 -- IMPROVE: use Constants.yaw instead of using _y directly
                                  & eThink .~ Just GameUtil.freeEdictA
                                  & eNextThink .~ levelTime + 10 + r' * 10)

    linkEntity selfRef

barrelDelay :: EntDie
barrelDelay =
  GenericEntDie "barrel_delay" $ \selfRef attackerRef _ _ _ -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT selfRef (\v -> v & eTakeDamage .~ Constants.damageNo
                                  & eNextThink .~ levelTime + 2 * Constants.frameTime
                                  & eThink .~ Just barrelExplode
                                  & eActivator .~ Just attackerRef)

barrelExplode :: EntThink
barrelExplode =
  GenericEntThink "barrel_explode" $ \selfRef -> do
    readEdictT selfRef >>= \self ->
      GameCombat.radiusDamage selfRef (fromJust $ self^.eActivator) (fromIntegral $ self^.eDmg) Nothing (fromIntegral (self^.eDmg) + 40) Constants.modBarrel

    self <- readEdictT selfRef

    let save = self^.eEntityState.esOrigin
        origin = (self^.eAbsMin) + fmap (* 0.5) (self^.eSize)

    -- a few big chunks
    let spd = 1.5 * fromIntegral (self^.eDmg) / 200
    modifyEdictT selfRef (\v -> v & eEntityState.esOrigin .~ origin)
    bigChunksDamage selfRef spd origin
    bigChunksDamage selfRef spd origin

    -- bottom corners
    let spd' = 1.75 * fromIntegral (self^.eDmg) / 200
        V3 a b c = self^.eAbsMin
    throwDebris selfRef "models/objects/debris3/tris.md2" spd' (V3 a b c)
    throwDebris selfRef "models/objects/debris3/tris.md2" spd' (V3 (a + (self^.eSize._x)) b c)
    throwDebris selfRef "models/objects/debris3/tris.md2" spd' (V3 a (b + (self^.eSize._y)) c)
    throwDebris selfRef "models/objects/debris3/tris.md2" spd' (V3 (a + (self^.eSize._x)) (b + (self^.eSize._y)) c)

    -- a bunch of little chunks
    let spd'' = 2 * fromIntegral (self^.eDmg) / 200
    littleChunksDamage selfRef spd'' origin
    littleChunksDamage selfRef spd'' origin
    littleChunksDamage selfRef spd'' origin
    littleChunksDamage selfRef spd'' origin
    littleChunksDamage selfRef spd'' origin
    littleChunksDamage selfRef spd'' origin
    littleChunksDamage selfRef spd'' origin
    littleChunksDamage selfRef spd'' origin

    modifyEdictT selfRef (\v ->v & eEntityState.esOrigin .~ save)

    readEdictT selfRef >>= \self ->
      case self^.eGroundEntity of
        Nothing -> becomeExplosion1 selfRef
        Just _ -> becomeExplosion2 selfRef

    return True

  where bigChunksDamage :: EdictReference -> Float -> V3 Float -> Quake ()
        bigChunksDamage selfRef spd origin = do
          self <- readEdictT selfRef

          r1 <- Lib.crandom
          r2 <- Lib.crandom
          r3 <- Lib.crandom

          let a = (origin^._x) + r1 * (self^.eSize._x)
              b = (origin^._y) + r2 * (self^.eSize._y)
              c = (origin^._z) + r3 * (self^.eSize._z)
              org = V3 a b c

          throwDebris selfRef "models/objects/debris1/tris.md2" spd org

        littleChunksDamage :: EdictReference -> Float -> V3 Float -> Quake ()
        littleChunksDamage selfRef spd origin = do
          self <- readEdictT selfRef

          r1 <- Lib.crandom
          r2 <- Lib.crandom
          r3 <- Lib.crandom

          let a = (origin^._x) + r1 * (self^.eSize._x)
              b = (origin^._y) + r2 * (self^.eSize._y)
              c = (origin^._z) + r3 * (self^.eSize._z)
              org = V3 a b c

          throwDebris selfRef "models/objects/debris2/tris.md2" spd org

{-
- QUAKED misc_explobox (0 .5 .8) (-16 -16 0) (16 16 40) Large exploding
- box. You can override its mass (100), health (80), and dmg (150).
-}
barrelTouch :: EntTouch
barrelTouch =
  GenericEntTouch "barrel_touch" $ \selfRef otherRef _ _ -> do
    other <- readEdictT otherRef

    unless (isNothing (other^.eGroundEntity) || (other^.eGroundEntity) == Just selfRef) $ do
      self <- readEdictT selfRef

      let ratio = fromIntegral (other^.eMass) / fromIntegral (self^.eMass)
          v = (self^.eEntityState.esOrigin) - (other^.eEntityState.esOrigin)

      void $ M.walkMove selfRef (Math3D.vectorYaw v) (20 * ratio * Constants.frameTime)

gibDie :: EntDie
gibDie =
  GenericEntDie "gib_die" $ \selfRef _ _ _ _ ->
    GameUtil.freeEdict selfRef

useAreaPortal :: EntUse
useAreaPortal =
  GenericEntUse "use_areaportal" $ \edictRef _ _ -> do
    modifyEdictT edictRef (\v -> v & eCount %~ (`xor` 1)) -- toggle state

    edict <- readEdictT edictRef
    setAreaPortalState <- use $ gameBaseGlobals.gbGameImport.giSetAreaPortalState

    setAreaPortalState (edict^.eStyle) ((edict^.eCount) /= 0)

funcExplosiveUse :: EntUse
funcExplosiveUse =
  FuncExplosiveUse "func_explosive_use" $ \selfRef otherRef _ -> do
    self <- readEdictT selfRef
    v3o <- use $ globals.vec3Origin

    die funcExplosiveExplode selfRef selfRef (fromJust otherRef) (self^.eHealth) v3o

funcExplosiveSpawn :: EntUse
funcExplosiveSpawn =
  GenericEntUse "func_explosive_spawn" $ \selfRef _ _ -> do
    modifyEdictT selfRef (\v -> v & eSolid .~ Constants.solidBsp
                                  & eSvFlags %~ (.&. (complement Constants.svfNoClient))
                                  & eUse .~ Nothing)

    GameUtil.killBox selfRef
    
    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

{-
- QUAKED func_explosive (0 .5 .8) ? Trigger_Spawn ANIMATED ANIMATED_FAST
- Any brush that you want to explode or break apart. If you want an
- ex0plosion, set dmg and it will do a radius explosion of that amount at
- the center of the bursh.
- 
- If targeted it will not be shootable.
- 
- health defaults to 100.
- 
- mass defaults to 75. This determines how much debris is emitted when it
- explodes. You get one large chunk per 100 of mass (up to 8) and one small
- chunk per 25 of mass (up to 16). So 800 gives the most.
-}
funcExplosiveExplode :: EntDie
funcExplosiveExplode =
  GenericEntDie "func_explosive_explode" $ \selfRef inflictorRef attackerRef damage point -> do
    self <- readEdictT selfRef

    -- bmodel origins are (0 0 0), we need to adjust that here
    let size = fmap (* 0.5) (self^.eSize)
        origin = (self^.eAbsMin) + size

    modifyEdictT selfRef (\v -> v & eEntityState.esOrigin .~ origin
                                  & eTakeDamage .~ Constants.damageNo)

    when ((self^.eDmg) /= 0) $
      GameCombat.radiusDamage selfRef attackerRef (fromIntegral $ self^.eDmg) Nothing (fromIntegral (self^.eDmg) + 40) Constants.modExplosive

    self' <- readEdictT selfRef
    inflictor <- readEdictT inflictorRef

    let velocity = fmap (* 150) (normalize ((self'^.eEntityState.esOrigin) - (inflictor^.eEntityState.esOrigin)))

    modifyEdictT selfRef (\v -> v & eVelocity .~ velocity)

    -- start chunks towards the center
    let mass = if (self'^.eMass) == 0 then 75 else self'^.eMass
        size' = fmap (* 0.5) size

    -- big chunks
    when (mass >= 100) $ do
      let count = mass `div` 100
          count' = if count > 8 then 8 else count

      explosionDebris selfRef "models/objects/debris1/tris.md2" 1 origin size' count'

    -- small chunks
    let count = mass `div` 25
        count' = if count > 16 then 16 else count

    explosionDebris selfRef "models/objects/debris2/tris.md2" 2 origin size' count'

    GameUtil.useTargets selfRef (Just attackerRef)
    
    self'' <- readEdictT selfRef
    
    if (self''^.eDmg) /= 0
      then becomeExplosion1 selfRef
      else GameUtil.freeEdict selfRef

  where explosionDebris :: EdictReference -> B.ByteString -> Float -> V3 Float -> V3 Float -> Int -> Quake ()
        explosionDebris selfRef modelName speed origin size count
          | count <= 0 = return ()
          | otherwise = do
              r1 <- Lib.crandom
              r2 <- Lib.crandom
              r3 <- Lib.crandom
              let a = (origin^._x) + r1 * (size^._x)
                  b = (origin^._y) + r2 * (size^._y)
                  c = (origin^._z) + r3 * (size^._z)

              throwDebris selfRef modelName speed (V3 a b c)
              explosionDebris selfRef modelName speed origin size (count - 1)

pointCombatTouch :: EntTouch
pointCombatTouch =
  GenericEntTouch "point_combat_touch" $ \selfRef otherRef plane surf -> do
    other <- readEdictT otherRef

    when ((other^.eMoveTarget) == Just selfRef) $ do
      self <- readEdictT selfRef

      if | isJust (self^.eTarget) -> do
             target <- GameBase.pickTarget (self^.eTarget)

             modifyEdictT otherRef (\v -> v & eTarget .~ (self^.eTarget)
                                            & eGoalEntity .~ target
                                            & eMoveTarget .~ target)

             when (isNothing target) $ do
               dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
               dprintf ((self^.eClassName) `B.append` " at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` " target " `B.append` (fromJust $ self^.eTarget) `B.append` " does not exist\n")
               modifyEdictT otherRef (\v -> v & eMoveTarget .~ Just selfRef)

             modifyEdictT selfRef (\v -> v & eTarget .~ Nothing)

         | (self^.eSpawnFlags) .&. 1 /= 0 && (other^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) == 0 -> do
             levelTime <- use $ gameBaseGlobals.gbLevel.llTime

             modifyEdictT otherRef (\v -> v & eMonsterInfo.miPauseTime .~ levelTime + 100000000
                                            & eMonsterInfo.miAIFlags %~ (.|. Constants.aiStandGround))
            
             void $ think (fromJust $ other^.eMonsterInfo.miStand) otherRef

         | otherwise ->
             return ()

      other' <- readEdictT otherRef

      when ((other'^.eMoveTarget) == Just selfRef) $ do
        modifyEdictT otherRef (\v -> v & eTarget .~ Nothing
                                       & eMoveTarget .~ Nothing
                                       & eGoalEntity .~ (other'^.eEnemy)
                                       & eMonsterInfo.miAIFlags %~ (.&. (complement Constants.aiCombatPoint)))

      when (isJust (self^.ePathTarget)) $ do
        let saveTarget = self^.eTarget

        modifyEdictT selfRef (\v -> v & eTarget .~ (self^.ePathTarget))

        enemyClient <- case other'^.eEnemy of
                         Nothing -> return Nothing
                         Just enemyRef -> do
                           enemy <- readEdictT enemyRef
                           return (enemy^.eClient)

        oldEnemyClient <- case other'^.eOldEnemy of
                            Nothing -> return Nothing
                            Just oldEnemyRef -> do
                              oldEnemy <- readEdictT oldEnemyRef
                              return (oldEnemy^.eClient)

        activatorClient <- case other'^.eActivator of
                             Nothing -> return Nothing
                             Just activatorRef -> do
                               activator <- readEdictT activatorRef
                               return (activator^.eClient)

        let activator = if | isJust enemyClient -> (other'^.eEnemy)
                           | isJust oldEnemyClient -> (other'^.eOldEnemy)
                           | isJust activatorClient -> (other'^.eActivator)
                           | otherwise -> Just otherRef

        GameUtil.useTargets selfRef activator
        modifyEdictT selfRef (\v -> v & eTarget .~ saveTarget)

{-
- QUAKED misc_strogg_ship (1 .5 0) (-16 -16 0) (16 16 32) This is a Storgg
- ship for the flybys. It is trigger_spawned, so you must have something
- use it for it to show up. There must be a path for it to follow once it
- is activated.
- 
- "speed" How fast it should fly
-}
miscStroggShipUse :: EntUse
miscStroggShipUse =
  GenericEntUse "misc_strogg_ship_use" $ \selfRef otherRef activatorRef -> do
    modifyEdictT selfRef (\v -> v & eSvFlags %~ (.&. (complement Constants.svfNoClient))
                                  & eUse .~ Just GameFunc.trainUse)

    entUse GameFunc.trainUse selfRef otherRef activatorRef

{-
- QUAKED func_wall (0 .5 .8) ? TRIGGER_SPAWN TOGGLE START_ON ANIMATED
- ANIMATED_FAST This is just a solid wall if not inhibited
- 
- TRIGGER_SPAWN the wall will not be present until triggered it will then
- blink in to existance; it will kill anything that was in it's way
- 
- TOGGLE only valid for TRIGGER_SPAWN walls this allows the wall to be
- turned on and off
- 
- START_ON only valid for TRIGGER_SPAWN walls the wall will initially be
- present
-}
funcWallUse :: EntUse
funcWallUse =
  GenericEntUse "func_wall_use" $ \selfRef _ _ -> do
    self <- readEdictT selfRef

    if (self^.eSolid) == Constants.solidNot
      then do
        modifyEdictT selfRef (\v -> v & eSolid .~ Constants.solidBsp
                                      & eSvFlags %~ (.&. (complement Constants.svfNoClient)))

        void $ GameUtil.killBox selfRef

      else
        modifyEdictT selfRef (\v -> v & eSolid .~ Constants.solidNot
                                      & eSvFlags %~ (.|. Constants.svfNoClient))

    linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
    linkEntity selfRef

    self' <- readEdictT selfRef

    when ((self'^.eSpawnFlags) .&. 2 == 0) $
      modifyEdictT selfRef (\v -> v & eUse .~ Nothing)

{-
- QUAKED misc_banner (1 .5 0) (-4 -4 -4) (4 4 4) The origin is the bottom
- of the banner. The banner is 128 tall.
-}
miscBannerThink :: EntThink
miscBannerThink =
  GenericEntThink "misc_banner_think" $ \edictRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    modifyEdictT edictRef (\v -> v & eEntityState.esFrame %~ (`mod` 16) . (+ 1)
                                   & eNextThink .~ levelTime + Constants.frameTime)
    return True

funcObjectRelease :: EntThink
funcObjectRelease =
  GenericEntThink "func_object_release" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMoveType .~ Constants.moveTypeToss
                                  & eTouch .~ Just funcObjectTouch)

    return True

funcObjectUse :: EntUse
funcObjectUse =
  GenericEntUse "func_object_use" $ \selfRef _ _ -> do
    modifyEdictT selfRef (\v -> v & eSolid .~ Constants.solidBsp
                                  & eSvFlags %~ (.&. (complement Constants.svfNoClient))
                                  & eUse .~ Nothing)

    GameUtil.killBox selfRef
    void $ think funcObjectRelease selfRef

{-
- QUAKED func_object (0 .5 .8) ? TRIGGER_SPAWN ANIMATED ANIMATED_FAST This
- is solid bmodel that will fall if it's support it removed.
-}
funcObjectTouch :: EntTouch
funcObjectTouch =
  GenericEntTouch "func_object_touch" $ \selfRef otherRef plane _ -> do
    -- TODO: jake2 checks if (plane == null)
    self <- readEdictT selfRef
    other <- readEdictT otherRef

    unless ((plane^.cpNormal._z) < 1.0 || (other^.eTakeDamage) == Constants.damageNo) $ do
      v3o <- use $ globals.vec3Origin
      GameCombat.damage otherRef selfRef selfRef v3o (self^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

{-
- QUAKED misc_blackhole (1 .5 0) (-8 -8 -8) (8 8 8)
-}
miscBlackHoleUse :: EntUse
miscBlackHoleUse =
  GenericEntUse "misc_blackhole_use" $ \edictRef _ _ ->
    GameUtil.freeEdict edictRef

miscBlackHoleThink :: EntThink
miscBlackHoleThink =
  GenericEntThink "misc_blackhole_think" $ \selfRef -> do
    self <- readEdictT selfRef
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT selfRef (\v -> v & eEntityState.esFrame %~ (\v -> if v + 1 < 19 then v + 1 else 0)
                                  & eNextThink .~ levelTime + Constants.frameTime)

    return True

{-
- QUAKED misc_eastertank (1 .5 0) (-32 -32 -16) (32 32 32)
-}
miscEasterTankThink :: EntThink
miscEasterTankThink =
  GenericEntThink " misc_eastertank_think" $ \selfRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT selfRef (\v -> v & eEntityState.esFrame %~ (\v -> if v + 1 < 293 then v + 1 else 254)
                                  & eNextThink .~ levelTime + Constants.frameTime)

    return True

{-
- QUAKED misc_easterchick (1 .5 0) (-32 -32 0) (32 32 32)
-}
miscEasterChickThink :: EntThink
miscEasterChickThink =
  GenericEntThink "misc_easterchick_think" $ \selfRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT selfRef (\v -> v & eEntityState.esFrame %~ (\v -> if v + 1 < 247 then v + 1 else 208)
                                  & eNextThink .~ levelTime + Constants.frameTime)

    return True

{-
- QUAKED misc_easterchick2 (1 .5 0) (-32 -32 0) (32 32 32)
-}
miscEasterChick2Think :: EntThink
miscEasterChick2Think =
  GenericEntThink "misc_easterchick2_think" $ \selfRef -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT selfRef (\v -> v & eEntityState.esFrame %~ (\v -> if v + 1 < 287 then v + 1 else 248)
                                  & eNextThink .~ levelTime + Constants.frameTime)

    return True

commanderBodyUse :: EntUse
commanderBodyUse =
  GenericEntUse "commander_body_use" $ \selfRef _ _ -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let sound = gameImport^.giSound
        soundIndex = gameImport^.giSoundIndex

    soundIdx <- soundIndex (Just "tank/pain.wav")
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT selfRef (\v -> v & eThink .~ Just commanderBodyThink
                                  & eNextThink .~ levelTime + Constants.frameTime)

    sound (Just selfRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0

{-
- QUAKED monster_commander_body (1 .5 0) (-32 -32 0) (32 32 48) Not really
- a monster, this is the Tank Commander's decapitated body. There should be
- a item_commander_head that has this as it's target.
-}
commanderBodyThink :: EntThink
commanderBodyThink =
  GenericEntThink "commander_body_think" $ \selfRef -> do
    self <- readEdictT selfRef

    if (self^.eEntityState.esFrame) + 1 < 24
      then do
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        modifyEdictT selfRef (\v -> v & eEntityState.esFrame +~ 1
                                      & eNextThink .~ levelTime + Constants.frameTime)

      else
        modifyEdictT selfRef (\v -> v & eEntityState.esFrame +~ 1
                                      & eNextThink .~ 0)

    when ((self^.eEntityState.esFrame) + 1 == 22) $ do
      gameImport <- use $ gameBaseGlobals.gbGameImport

      let sound = gameImport^.giSound
          soundIndex = gameImport^.giSoundIndex

      soundIdx <- soundIndex (Just "tank/thud.wav")
      sound (Just selfRef) Constants.chanBody soundIdx 1 Constants.attnNorm 0

    return True

commanderBodyDrop :: EntThink
commanderBodyDrop =
  GenericEntThink "commander_body_drop" $ \selfRef -> do
    modifyEdictT selfRef (\v -> v & eMoveType .~ Constants.moveTypeToss
                                  & eEntityState.esOrigin._z +~ 2)

    return True

throwDebris :: EdictReference -> B.ByteString -> Float -> V3 Float -> Quake ()
throwDebris selfRef modelName speed origin = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    chunkRef <- GameUtil.spawn
    self <- readEdictT selfRef

    c1 <- Lib.crandom
    c2 <- Lib.crandom
    c3 <- Lib.crandom

    let a = V3 (100 * c1) (100 * c2) (100 + 100 * c3)

    r1 <- Lib.randomF
    r2 <- Lib.randomF
    r3 <- Lib.randomF

    let avelocity = V3 (r1 * 600) (r2 * 600) (r3 * 600)

    r <- Lib.randomF

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    modifyEdictT chunkRef (\v -> v & eEntityState.esOrigin .~ origin
                                   & eVelocity .~ (self^.eVelocity) + fmap (* speed) a
                                   & eMoveType .~ Constants.moveTypeBounce
                                   & eSolid .~ Constants.solidNot
                                   & eAVelocity .~ avelocity
                                   & eThink .~ Just GameUtil.freeEdictA
                                   & eNextThink .~ levelTime + 5 + r * 5
                                   & eEntityState.esFrame .~ 0
                                   & eFlags .~ 0
                                   & eClassName .~ "debris"
                                   & eTakeDamage .~ Constants.damageYes
                                   & eDie .~ Just debrisDie)

    setModel chunkRef (Just modelName)

    linkEntity chunkRef

becomeExplosion1 :: EdictReference -> Quake ()
becomeExplosion1 selfRef = do
    self <- readEdictT selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let writeByte = gameImport^.giWriteByte
        writePosition = gameImport^.giWritePosition
        multicast = gameImport^.giMulticast

    writeByte Constants.svcTempEntity
    writeByte Constants.teExplosion1
    writePosition (self^.eEntityState.esOrigin)
    multicast (self^.eEntityState.esOrigin) Constants.multicastPvs

    GameUtil.freeEdict selfRef

becomeExplosion2 :: EdictReference -> Quake ()
becomeExplosion2 selfRef = do
    self <- readEdictT selfRef
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let writeByte = gameImport^.giWriteByte
        writePosition = gameImport^.giWritePosition
        multicast = gameImport^.giMulticast

    writeByte Constants.svcTempEntity
    writeByte Constants.teExplosion2
    writePosition (self^.eEntityState.esOrigin)
    multicast (self^.eEntityState.esOrigin) Constants.multicastPvs

    GameUtil.freeEdict selfRef

debrisDie :: EntDie
debrisDie =
  GenericEntDie "debris_die" $ \selfRef _ _ _ _ ->
    GameUtil.freeEdict selfRef

throwClientHead :: EdictReference -> Int -> Quake ()
throwClientHead _ _ = do
    io (putStrLn "GameMisc.throwClientHead") >> undefined -- TODO
