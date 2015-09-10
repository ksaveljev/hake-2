{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameMisc where

import Control.Lens (use, preuse, (^.), ix, (.=), zoom, (%=), (&), (+~), (+=))
import Control.Monad (liftM, when, void, unless)
import Data.Bits ((.|.), (.&.), complement)
import Data.Char (ord)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3(..), _x, _y, _z)
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
  GenericEntUse "light_use" $ \(EdictReference selfIdx) _ _ -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    configString <- use $ gameBaseGlobals.gbGameImport.giConfigString

    if (self^.eSpawnFlags) .&. startOff /= 0
      then do
        configString (Constants.csLights + (self^.eStyle)) "m"
        gameBaseGlobals.gbGEdicts.ix selfIdx.eSpawnFlags %= (.&. (complement startOff))
      else do
        configString (Constants.csLights + (self^.eStyle)) "a"
        gameBaseGlobals.gbGEdicts.ix selfIdx.eSpawnFlags %= (.|. startOff)

spPathCorner :: EdictReference -> Quake ()
spPathCorner er@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let dprintf = gameImport^.giDprintf
        linkEntity = gameImport^.giLinkEntity

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    if isNothing (edict^.eTargetName)
      then do
        dprintf $ "path_corner with no targetname at " `B.append` BC.pack (show (edict^.eEntityState.esOrigin)) `B.append` "\n"
        GameUtil.freeEdict er
      else do
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eSolid .= Constants.solidTrigger
          eTouch .= Just pathCornerTouch
          eMins .= V3 (-8) (-8) (-8)
          eMaxs .= V3 8 8 8
          eSvFlags %= (.|. Constants.svfNoClient)

        linkEntity er

{-
- QUAKED path_corner (.5 .3 0) (-8 -8 -8) (8 8 8) TELEPORT Target: next
- path corner Pathtarget: gets used when an entity that has this
- path_corner targeted touches it
-}
pathCornerTouch :: EntTouch
pathCornerTouch =
  GenericEntTouch "path_corner_touch" $ \selfRef@(EdictReference selfIdx) otherRef@(EdictReference otherIdx) _ _ -> do
    done <- shouldReturn selfRef otherRef

    unless done $ do
      saveTarget selfRef otherRef
      target <- pickTarget selfRef

      nextTarget <- if isJust target
                      then pickNextTarget (fromJust target) otherRef
                      else return Nothing

      zoom (gameBaseGlobals.gbGEdicts.ix otherIdx) $ do
        eGoalEntity .= nextTarget
        eMoveTarget .= nextTarget

      Just wait <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eWait
      if wait /= 0
        then do
          time <- use $ gameBaseGlobals.gbLevel.llTime
          Just stand <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx.eMonsterInfo.miStand
          gameBaseGlobals.gbGEdicts.ix otherIdx.eMonsterInfo.miPauseTime .= time + wait
          void $ think (fromJust stand) otherRef
        else do
          Just other <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx

          if isNothing (other^.eMoveTarget)
            then do
              time <- use $ gameBaseGlobals.gbLevel.llTime
              gameBaseGlobals.gbGEdicts.ix otherIdx.eMonsterInfo.miPauseTime .= time + 100000000
              void $ think (fromJust $ other^.eMonsterInfo.miStand) otherRef
            else do
              let Just (EdictReference goalIdx) = other^.eGoalEntity
              Just goal <- preuse $ gameBaseGlobals.gbGEdicts.ix goalIdx
              let v = (goal^.eEntityState.esOrigin) - (other^.eEntityState.esOrigin)
              gameBaseGlobals.gbGEdicts.ix otherIdx.eIdealYaw .= Math3D.vectorYaw v

  where shouldReturn :: EdictReference -> EdictReference -> Quake Bool
        shouldReturn selfRef (EdictReference otherIdx) = do
          Just moveTarget <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx.eMoveTarget
          Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx.eEnemy

          if moveTarget /= (Just selfRef) || isJust enemy
            then return True
            else return False

        saveTarget :: EdictReference -> EdictReference -> Quake ()
        saveTarget selfRef@(EdictReference selfIdx) otherRef = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
          let target = self^.eTarget
          
          gameBaseGlobals.gbGEdicts.ix selfIdx.eTarget .= self^.ePathTarget
          GameUtil.useTargets selfRef (Just otherRef)
          gameBaseGlobals.gbGEdicts.ix selfIdx.eTarget .= target

        pickTarget :: EdictReference -> Quake (Maybe EdictReference)
        pickTarget (EdictReference selfIdx) = do
          Just target <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx.eTarget
          if isJust target
            then GameBase.pickTarget target
            else return Nothing

        pickNextTarget :: EdictReference -> EdictReference -> Quake (Maybe EdictReference)
        pickNextTarget edictRef@(EdictReference edictIdx) (EdictReference otherIdx) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          if (edict^.eSpawnFlags) .&. 1 /= 0
            then do
              Just other <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx
              let v = edict^.eEntityState.esOrigin
                  -- v[2] += next.mins[2];
                  -- v[2] -= other.mins[2];
                  v' = v & _z +~ ((edict^.eMins._z) - (other^.eMins._z))
              gameBaseGlobals.gbGEdicts.ix otherIdx.eEntityState.esOrigin .= v'
              next <- GameBase.pickTarget (edict^.eTarget)
              gameBaseGlobals.gbGEdicts.ix otherIdx.eEntityState.esEvent .= Constants.evOtherTeleport
              return next
            else return (Just edictRef)

spPointCombat :: EdictReference -> Quake ()
spPointCombat er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then GameUtil.freeEdict er
      else do
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eSolid .= Constants.solidTrigger
          eTouch .= Just pointCombatTouch
          eMins .= V3 (-8) (-8) (-16)
          eMaxs .= V3 8 8 16
          eSvFlags .= Constants.svfNoClient

        linkEntity <- use $ gameBaseGlobals.gbGameImport.giLinkEntity
        linkEntity er

spViewThing :: EdictReference -> Quake ()
spViewThing edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let dprintf = gameImport^.giDprintf
        linkEntity = gameImport^.giLinkEntity
        modelIndex = gameImport^.giModelIndex

    modelIdx <- modelIndex (Just "models/objects/banner/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypeNone
      eSolid .= Constants.solidBbox
      eEntityState.esRenderFx .= Constants.rfFrameLerp
      eMins .= V3 (-16) (-16) (-24)
      eMaxs .= V3 16 16 32
      eEntityState.esModelIndex .= modelIdx

    linkEntity edictRef

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eNextThink .= levelTime + 0.5
      eThink .= Just thViewThing

{-
- QUAKED viewthing (0 .5 .8) (-8 -8 -8) (8 8 8) Just for the debugging
- level. Don't use
-}
thViewThing :: EntThink
thViewThing =
  GenericEntThink "th_viewthing" $ \(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eEntityState.esFrame .= ((edict^.eEntityState.esFrame) + 1) `mod` 7
      eNextThink .= levelTime + Constants.frameTime

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
spInfoNotNull (EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eAbsMin .= (edict^.eEntityState.esOrigin)
      eAbsMax .= (edict^.eEntityState.esOrigin)

spLight :: EdictReference -> Quake ()
spLight er@(EdictReference edictIdx) = do
    -- no targeted lights in deathmatch, because they cause global messages
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if isNothing (edict^.eTargetName) || deathmatchValue /= 0
      then GameUtil.freeEdict er
      else 
        when ((edict^.eStyle) >= 32) $ do
          gameBaseGlobals.gbGEdicts.ix edictIdx.eUse .= Just lightUse

          configString <- use $ gameBaseGlobals.gbGameImport.giConfigString

          if (edict^.eSpawnFlags) .&. startOff /= 0
            then configString (Constants.csLights + (edict^.eStyle)) "a"
            else configString (Constants.csLights + (edict^.eStyle)) "m"

spFuncWall :: EdictReference -> Quake ()
spFuncWall edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let linkEntity = gameImport^.giLinkEntity
        setModel = gameImport^.giSetModel

    preuse (gameBaseGlobals.gbGEdicts.ix edictIdx) >>= \(Just edict) ->
      setModel edictRef (edict^.eiModel)

    gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveType .= Constants.moveTypePush

    updateEdictEffects

    isAWall <- checkWall

    if isAWall
      then do
        -- just a wall
        gameBaseGlobals.gbGEdicts.ix edictIdx.eSolid .= Constants.solidBsp
        linkEntity edictRef
      else do
        -- it must be TRIGGER_SPAWN
        checkTriggerSpawn

        -- yell if the spawnflags are odd
        checkOddSpawnFlags

        gameBaseGlobals.gbGEdicts.ix edictIdx.eUse .= Just funcWallUse

        preuse (gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags) >>= \(Just v) ->
          if v .&. 4 /= 0
            then
              gameBaseGlobals.gbGEdicts.ix edictIdx.eSolid .= Constants.solidBsp
            else do
              gameBaseGlobals.gbGEdicts.ix edictIdx.eSolid .= Constants.solidNot
              gameBaseGlobals.gbGEdicts.ix edictIdx.eSvFlags %= (.|. Constants.svfNoClient)

        linkEntity edictRef

  where updateEdictEffects :: Quake ()
        updateEdictEffects = do
          Just spawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags

          when (spawnFlags .&. 8 /= 0) $
            gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEffects %= (.|. Constants.efAnimAll)

          when (spawnFlags .&. 16 /= 0) $
            gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEffects %= (.|. Constants.efAnimAllFast)

        checkWall :: Quake Bool
        checkWall = do
          Just spawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags
          return $ spawnFlags .&. 7 == 0

        checkTriggerSpawn :: Quake ()
        checkTriggerSpawn = do
          Just spawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags
          when (spawnFlags .&. 1 == 0) $ do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf "func_wall missing TRIGGER_SPAWN\n"
            gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags %= (.|. 1)

        checkOddSpawnFlags :: Quake ()
        checkOddSpawnFlags = do
          Just spawnFlags <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags

          when (spawnFlags .&. 4 /= 0 && spawnFlags .&. 2 == 0) $ do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf "func_wall START_ON without TOGGLE\n"
            gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags %= (.|. 2)

spFuncObject :: EdictReference -> Quake ()
spFuncObject selfRef@(EdictReference selfIdx) = do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
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

        zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
          eMins .= mins
          eMaxs .= maxs
          eDmg .= dmg
          eEntityState.esEffects .= effects'
          eClipMask .= Constants.maskMonsterSolid
          eSolid .= Constants.solidBsp
          eMoveType .= Constants.moveTypePush
          eThink .= Just funcObjectRelease
          eNextThink .= levelTime + 2 * Constants.frameTime

      else
        zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
          eMins .= mins
          eMaxs .= maxs
          eDmg .= dmg
          eEntityState.esEffects .= effects'
          eClipMask .= Constants.maskMonsterSolid
          eSolid .= Constants.solidNot
          eMoveType .= Constants.moveTypePush
          eUse .= Just funcObjectUse
          eSvFlags %= (.|. Constants.svfNoClient)

    linkEntity selfRef

spFuncExplosive :: EdictReference -> Quake ()
spFuncExplosive er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then GameUtil.freeEdict er
      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport
        let modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity
            setModel = gameImport^.giSetModel

        gameBaseGlobals.gbGEdicts.ix edictIdx.eMoveType .= Constants.moveTypePush

        void $ modelIndex (Just "models/objects/debris1/tris.md2")
        void $ modelIndex (Just "models/objects/debris2/tris.md2")

        Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

        setModel er (edict^.eiModel)

        if (edict^.eSpawnFlags) .&. 1 /= 0
          then
            zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
              eSvFlags %= (.|. Constants.svfNoClient)
              eSolid .= Constants.solidNot
              eUse .= Just funcExplosiveSpawn
          else do
            gameBaseGlobals.gbGEdicts.ix edictIdx.eSolid .= Constants.solidBsp
            when (isJust (edict^.eTargetName)) $
              gameBaseGlobals.gbGEdicts.ix edictIdx.eUse .= Just funcExplosiveUse

        when ((edict^.eSpawnFlags) .&. 2 /= 0) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEffects %= (.|. Constants.efAnimAll)

        when ((edict^.eSpawnFlags) .&. 4 /= 0) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esEffects %= (.|. Constants.efAnimAllFast)

        Just edictUse <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eUse
        case edictUse of
          Just (FuncExplosiveUse _ _) -> return ()
          _ -> do
            when ((edict^.eHealth) == 0) $
              gameBaseGlobals.gbGEdicts.ix edictIdx.eHealth .= 100

            zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
              eDie .= Just funcExplosiveExplode
              eTakeDamage .= Constants.damageYes

        linkEntity er

spMiscExploBox :: EdictReference -> Quake ()
spMiscExploBox er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then GameUtil.freeEdict er
      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport
        let modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        void $ modelIndex (Just "models/objects/debris1/tris.md2")
        void $ modelIndex (Just "models/objects/debris2/tris.md2")
        void $ modelIndex (Just "models/objects/debris3/tris.md2")

        let trisModel = "models/objects/barrels/tris.md2"
        tris <- modelIndex (Just trisModel)

        time <- use $ gameBaseGlobals.gbLevel.llTime

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eSolid .= Constants.solidBbox
          eMoveType .= Constants.moveTypeStep
          eiModel .= Just trisModel
          eEntityState.esModelIndex .= tris
          eMins .= V3 (-16) (-16) 0
          eMaxs .= V3 16 16 40
          eDie .= Just barrelDelay
          eTakeDamage .= Constants.damageYes
          eMonsterInfo.miAIFlags .= Constants.aiNoStep
          eTouch .= Just barrelTouch
          eThink .= Just M.dropToFloor
          eNextThink .= time + 2 * Constants.frameTime

        Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

        when ((edict^.eMass) == 0) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eMass .= 400

        when ((edict^.eHealth) == 0) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eHealth .= 10

        when ((edict^.eDmg) == 0) $
          gameBaseGlobals.gbGEdicts.ix edictIdx.eDmg .= 150

        linkEntity er

spMiscBlackHole :: EdictReference -> Quake ()
spMiscBlackHole edictRef@(EdictReference edictIdx) = do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    mIdx <- modelIndex (Just "models/objects/black/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypeNone
      eSolid .= Constants.solidNot
      eMins .= V3 (-64) (-64) 0
      eMaxs .= V3 64 64 8
      eEntityState.esModelIndex .= mIdx
      eEntityState.esRenderFx .= Constants.rfTranslucent
      eUse .= Just miscBlackHoleUse
      eThink .= Just miscBlackHoleThink
      eNextThink .= levelTime + 2 * Constants.frameTime

    linkEntity edictRef

spMiscEasterTank :: EdictReference -> Quake ()
spMiscEasterTank edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    mIdx <- modelIndex (Just "models/monsters/tank/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypeNone
      eSolid .= Constants.solidBbox
      eMins .= V3 (-32) (-32) (-16)
      eMaxs .= V3 32 32 32
      eEntityState.esModelIndex .= mIdx
      eEntityState.esFrame .= 254
      eThink .= Just miscEasterTankThink
      eNextThink .= levelTime + 2 * Constants.frameTime

    linkEntity edictRef

spMiscEasterChick :: EdictReference -> Quake ()
spMiscEasterChick edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    mIdx <- modelIndex (Just "models/monsters/bitch/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypeNone
      eSolid .= Constants.solidBbox
      eMins .= V3 (-32) (-32) 0
      eMaxs .= V3 32 32 32
      eEntityState.esModelIndex .= mIdx
      eEntityState.esFrame .= 208
      eThink .= Just miscEasterChickThink
      eNextThink .= levelTime + 2 * Constants.frameTime

    linkEntity edictRef

spMiscEasterChick2 :: EdictReference -> Quake ()
spMiscEasterChick2 edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    mIdx <- modelIndex (Just "models/monsters/bitch/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypeNone
      eSolid .= Constants.solidBbox
      eMins .= V3 (-32) (-32) 0
      eMaxs .= V3 32 32 32
      eEntityState.esModelIndex .= mIdx
      eEntityState.esFrame .= 248
      eThink .= Just miscEasterChick2Think
      eNextThink .= levelTime + 2 * Constants.frameTime

    linkEntity edictRef

spMonsterCommanderBody :: EdictReference -> Quake ()
spMonsterCommanderBody selfRef@(EdictReference selfIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity
        soundIndex = gameImport^.giSoundIndex

    mIdx <- modelIndex (Just "models/monsters/commandr/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMoveType .= Constants.moveTypeNone
      eSolid .= Constants.solidBbox
      eiModel .= Just "models/monsters/commandr/tris.md2"
      eEntityState.esModelIndex .= mIdx
      eMins .= V3 (-32) (-32) 0
      eMaxs .= V3 32 32 48
      eUse .= Just commanderBodyUse
      eTakeDamage .= Constants.damageYes
      eFlags .= Constants.flGodMode
      eEntityState.esRenderFx %= (.|. Constants.rfFrameLerp)

    linkEntity selfRef
    
    void $ soundIndex (Just "tank/thud.wav")
    void $ soundIndex (Just "tank/pain.wav")

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eThink .= Just commanderBodyDrop
      eNextThink .= levelTime + 5 * Constants.frameTime

spMiscBanner :: EdictReference -> Quake ()
spMiscBanner edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    tris <- modelIndex (Just "models/objects/banner/tris.md2")
    r <- Lib.rand
    time <- use $ gameBaseGlobals.gbLevel.llTime

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypeNone
      eSolid .= Constants.solidNot
      eEntityState.esModelIndex .= tris
      eEntityState.esFrame .= (fromIntegral r) `mod` 16

    linkEntity edictRef

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eThink .= Just miscBannerThink
      eNextThink .= time + Constants.frameTime

spMiscDeadSoldier :: EdictReference -> Quake ()
spMiscDeadSoldier er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then GameUtil.freeEdict er
      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport
        let modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        tris <- modelIndex (Just "models/deadbods/dude/tris.md2")

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eMoveType .= Constants.moveTypeNone
          eSolid .= Constants.solidBbox
          eEntityState.esModelIndex .= tris

        Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
        let spawnFlags = edict^.eSpawnFlags

        -- defaults to frame 0
        let frame = if | spawnFlags .&. 2 /= 0 -> 1
                       | spawnFlags .&. 4 /= 0 -> 2
                       | spawnFlags .&. 8 /= 0 -> 3
                       | spawnFlags .&. 16 /= 0 -> 4
                       | spawnFlags .&. 32 /= 0 -> 5
                       | otherwise -> 0

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eEntityState.esFrame .= frame
          eMins .= V3 (-16) (-16) 0
          eMaxs .= V3 16 16 16
          eDeadFlag .= Constants.deadDead
          eTakeDamage .= Constants.damageYes
          eSvFlags %= (.|. (Constants.svfMonster .|. Constants.svfDeadMonster))
          eDie .= Just miscDeadSoldierDie
          eMonsterInfo.miAIFlags %= (.|. Constants.aiGoodGuy)

        linkEntity er

spMiscViper :: EdictReference -> Quake ()
spMiscViper edictRef@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
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

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eSpeed .= speed
          eMoveType .= Constants.moveTypePush
          eSolid .= Constants.solidNot
          eEntityState.esModelIndex .= modelIdx
          eMins .= V3 (-16) (-16) 0
          eMaxs .= V3 16 16 32
          eThink .= Just GameFunc.funcTrainFind
          eNextThink .= levelTime + Constants.frameTime
          eUse .= Just miscViperUse
          eSvFlags %= (.|. Constants.svfNoClient)
          eMoveInfo.miAccel .= speed
          eMoveInfo.miDecel .= speed
          eMoveInfo.miSpeed .= speed

        linkEntity edictRef

miscViperUse :: EntUse
miscViperUse =
  GenericEntUse "misc_viper_use" $ \selfRef@(EdictReference selfIdx) otherRef activatoRef -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eSvFlags %= (.&. (complement Constants.svfNoClient))
      eUse .= Just GameFunc.trainUse

    entUse (GameFunc.trainUse) selfRef otherRef activatoRef

{-
- QUAKED misc_bigviper (1 .5 0) (-176 -120 -24) (176 120 72) This is a
- large stationary viper as seen in Paul's intro
-}
spMiscBigViper :: EdictReference -> Quake ()
spMiscBigViper edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/ships/bigviper/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypeNone
      eSolid .= Constants.solidBbox
      eMins .= V3 (-176) (-120) (-24)
      eMaxs .= V3 176 120 72
      eEntityState.esModelIndex .= modelIdx

    linkEntity edictRef

spMiscViperBomb :: EdictReference -> Quake ()
spMiscViperBomb selfRef@(EdictReference selfIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/objects/bomb/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMoveType .= Constants.moveTypeNone
      eSolid .= Constants.solidNot
      eMins .= V3 (-8) (-8) (-8)
      eMaxs .= V3 8 8 8
      eEntityState.esModelIndex .= modelIdx
      eDmg %= (\v -> if v == 0 then 1000 else v)
      eUse .= Just miscViperBombUse
      eSvFlags %= (.|. Constants.svfNoClient)

    linkEntity selfRef

miscViperBombUse :: EntUse
miscViperBombUse =
  GenericEntUse "misc_viper_bomb_use" $ \selfRef@(EdictReference selfIdx) _ activatorRef -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eSolid .= Constants.solidBbox
      eSvFlags %= (.&. (complement Constants.svfNoClient))
      eEntityState.esEffects %= (.|. Constants.efRocket)
      eUse .= Nothing
      eMoveType .= Constants.moveTypeToss
      ePrethink .= Just miscViperBombPrethink
      eTouch .= Just miscViperBombTouch
      eActivator .= activatorRef

    es <- GameBase.gFind Nothing GameBase.findByClass "misc_viper"

    case es of
      Nothing -> return () -- jake2 doesn't do it though
      Just (EdictReference viperIdx) -> do
        Just viper <- preuse $ gameBaseGlobals.gbGEdicts.ix viperIdx
        levelTime <- use $ gameBaseGlobals.gbLevel.llTime

        zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
          eVelocity .= fmap (* (viper^.eMoveInfo.miSpeed)) (viper^.eMoveInfo.miDir)
          eTimeStamp .= levelTime
          eMoveInfo.miDir .= (viper^.eMoveInfo.miDir)

miscViperBombPrethink :: EntThink
miscViperBombPrethink =
  GenericEntThink "misc_viper_bomb_prethink" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    let diff = (self^.eTimeStamp) - levelTime
        diff' = if diff < (-1.0) then -1.0 else diff
        V3 a b c = fmap (* (1.0 + diff')) (self^.eMoveInfo.miDir)
        V3 a' b' c' = Math3D.vectorAngles (V3 a b diff')

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eGroundEntity .= Nothing
      eEntityState.esAngles .= V3 a' b' ((self^.eEntityState.esAngles._z) + 10)

    return True

{-
- QUAKED misc_viper_bomb (1 0 0) (-8 -8 -8) (8 8 8) "dmg" how much boom
- should the bomb make?
-}
miscViperBombTouch :: EntTouch
miscViperBombTouch =
  GenericEntTouch "misc_viper_bomb_touch" $ \selfRef@(EdictReference selfIdx) _ _ _ -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    GameUtil.useTargets selfRef (self^.eActivator)

    gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esOrigin._z .= (self^.eAbsMin._z) + 1

    GameCombat.radiusDamage selfRef selfRef (fromIntegral $ self^.eDmg) Nothing (fromIntegral (self^.eDmg) + 40) Constants.modBomb
    becomeExplosion2 selfRef

spMiscStroggShip :: EdictReference -> Quake ()
spMiscStroggShip edictRef@(EdictReference edictIdx) = do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
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
        tris <- modelIndex (Just "models/ships/strogg1/tris.md2")
        time <- use $ gameBaseGlobals.gbLevel.llTime

        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eMoveType .= Constants.moveTypePush
          eSolid .= Constants.solidNot
          eEntityState.esModelIndex .= tris
          eMins .= V3 (-16) (-16) 0
          eMaxs .= V3 16 16 32

          eThink .= Just (GameFunc.funcTrainFind)
          eNextThink .= time + Constants.frameTime
          eUse .= Just (miscStroggShipUse)
          eSvFlags %= (.|. Constants.svfNoClient)
          eMoveInfo.miAccel .= (edict^.eSpeed)
          eMoveInfo.miDecel .= (edict^.eSpeed)
          eMoveInfo.miSpeed .= (edict^.eSpeed)

        linkEntity edictRef

spMiscSatelliteDish :: EdictReference -> Quake ()
spMiscSatelliteDish edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/objects/satellite/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypeNone
      eSolid .= Constants.solidBbox
      eMins .= V3 (-64) (-64) 0
      eMaxs .= V3 64 64 128
      eEntityState.esModelIndex .= modelIdx
      eUse .= Just miscSatelliteDishUse

    linkEntity edictRef

miscSatelliteDishUse :: EntUse
miscSatelliteDishUse =
  GenericEntUse "misc_satellite_dish_use" $ \selfRef@(EdictReference selfIdx) _ _ -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eEntityState.esFrame .= 0
      eThink .= Just miscSatelliteDishThink
      eNextThink .= levelTime + Constants.frameTime

{-
- QUAKED misc_satellite_dish (1 .5 0) (-64 -64 0) (64 64 128)
-}
miscSatelliteDishThink :: EntThink
miscSatelliteDishThink =
  GenericEntThink "misc_satellite_dish_think" $ \selfRef@(EdictReference selfIdx) -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    if (self^.eEntityState.esFrame) + 1 < 38
      then
        zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
          eEntityState.esFrame += 1
          eNextThink .= levelTime + Constants.frameTime
      else
        gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esFrame += 1

    return True

{-
- QUAKED light_mine1 (0 1 0) (-2 -2 -12) (2 2 12)
-}
spLightMine1 :: EdictReference -> Quake ()
spLightMine1 edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/objects/minelite/light1/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypeNone
      eSolid .= Constants.solidBbox
      eEntityState.esModelIndex .= modelIdx

    linkEntity edictRef

{-
- QUAKED light_mine2 (0 1 0) (-2 -2 -12) (2 2 12)
-}
spLightMine2 :: EdictReference -> Quake ()
spLightMine2 edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let modelIndex = gameImport^.giModelIndex
        linkEntity = gameImport^.giLinkEntity

    modelIdx <- modelIndex (Just "models/objects/minelite/light2/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eMoveType .= Constants.moveTypeNone
      eSolid .= Constants.solidBbox
      eEntityState.esModelIndex .= modelIdx
    
    linkEntity edictRef

{-
- QUAKED misc_gib_arm (1 0 0) (-8 -8 -8) (8 8 8) Intended for use with the
- target_spawner
-}
spMiscGibArm :: EdictReference -> Quake ()
spMiscGibArm edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    r1 <- Lib.randomF
    r2 <- Lib.randomF
    r3 <- Lib.randomF

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eSolid .= Constants.solidNot
      eEntityState.esEffects %= (.|. Constants.efGib)
      eTakeDamage .= Constants.damageYes
      eDie .= Just gibDie
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfMonster)
      eDeadFlag .= Constants.deadDead
      eAVelocity .= V3 (r1 * 200) (r2 * 200) (r3 * 200)
      eThink .= Just GameUtil.freeEdictA
      eNextThink .= levelTime + 30

    setModel edictRef (Just "models/objects/gibs/arm/tris.md2")

    linkEntity edictRef

{-
- QUAKED misc_gib_leg (1 0 0) (-8 -8 -8) (8 8 8) Intended for use with the
- target_spawner
-}
spMiscGibLeg :: EdictReference -> Quake ()
spMiscGibLeg edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    r1 <- Lib.randomF
    r2 <- Lib.randomF
    r3 <- Lib.randomF

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eSolid .= Constants.solidNot
      eEntityState.esEffects %= (.|. Constants.efGib)
      eTakeDamage .= Constants.damageYes
      eDie .= Just gibDie
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfMonster)
      eDeadFlag .= Constants.deadDead
      eAVelocity .= V3 (r1 * 200) (r2 * 200) (r3 * 200)
      eThink .= Just GameUtil.freeEdictA
      eNextThink .= levelTime + 30

    setModel edictRef (Just "models/objects/gibs/leg/tris.md2")

    linkEntity edictRef

{-
- QUAKED misc_gib_head (1 0 0) (-8 -8 -8) (8 8 8) Intended for use with the
- target_spawner
-}
spMiscGibHead :: EdictReference -> Quake ()
spMiscGibHead edictRef@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    r1 <- Lib.randomF
    r2 <- Lib.randomF
    r3 <- Lib.randomF

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    
    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eSolid .= Constants.solidNot
      eEntityState.esEffects %= (.|. Constants.efGib)
      eTakeDamage .= Constants.damageYes
      eDie .= Just gibDie
      eMoveType .= Constants.moveTypeToss
      eSvFlags %= (.|. Constants.svfMonster)
      eDeadFlag .= Constants.deadDead
      eAVelocity .= V3 (r1 * 200) (r2 * 200) (r3 * 200)
      eThink .= Just GameUtil.freeEdictA
      eNextThink .= levelTime + 30

    setModel edictRef (Just "models/objects/gibs/head/tris.md2")

    linkEntity edictRef

{-
- QUAKED target_character (0 0 1) ? used with target_string (must be on
- same "team") "count" is position in the string (starts at 1)
-}
spTargetCharacter :: EdictReference -> Quake ()
spTargetCharacter selfRef@(EdictReference selfIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMoveType .= Constants.moveTypePush
      eSolid .= Constants.solidBsp
      eEntityState.esFrame .= 12

    setModel selfRef (self^.eiModel)

    linkEntity selfRef

spTargetString :: EdictReference -> Quake ()
spTargetString selfRef@(EdictReference selfIdx) = do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMessage %= (\v -> if isNothing v then Just "" else v)
      eUse .= Just targetStringUse

{-
- QUAKED target_string (0 0 1) (-8 -8 -8) (8 8 8)
-}
targetStringUse :: EntUse
targetStringUse =
  GenericEntUse "target_string_use" $ \selfRef@(EdictReference selfIdx) _ _ -> do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let Just message = self^.eMessage

    setFrame message (self^.eTeamMaster)

  where setFrame :: B.ByteString -> Maybe EdictReference -> Quake ()
        setFrame _ Nothing = return ()
        setFrame message (Just (EdictReference edictIdx)) = do
          Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

          if (edict^.eCount) == 0
            then
              setFrame message (edict^.eTeamChain)

            else do
              let n = (edict^.eCount) - 1

              if n >= B.length message
                then do
                  gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= 12
                  setFrame message (edict^.eTeamChain)

                else do
                  let c = message `BC.index` n
                      frame = if | c >= '0' && c <= '9' -> ord c - ord '0'
                                 | c == '-' -> 10
                                 | c == ':' -> 11
                                 | otherwise -> 12

                  gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame .= frame
                  setFrame message (edict^.eTeamChain)

spFuncClock :: EdictReference -> Quake ()
spFuncClock selfRef@(EdictReference selfIdx) = do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

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
             gameBaseGlobals.gbGEdicts.ix selfIdx.eCount .= 60 * 60

           funcClockReset selfRef

           zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
             eMessage .= Just ""
             eThink .= Just funcClockThink

           Just self' <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

           if (self'^.eSpawnFlags) .&. 4 /= 0
             then 
               gameBaseGlobals.gbGEdicts.ix selfIdx.eUse .= Just funcClockUse
             else do
               levelTime <- use $ gameBaseGlobals.gbLevel.llTime
               gameBaseGlobals.gbGEdicts.ix selfIdx.eNextThink .= levelTime + 1

funcClockReset :: EdictReference -> Quake ()
funcClockReset _ = do
    io (putStrLn "GameMisc.funcClockReset") >> undefined -- TODO

funcClockThink :: EntThink
funcClockThink =
  GenericEntThink "func_clock_think" $ \_ -> do
    io (putStrLn "GameMisc.funcClockThink") >> undefined -- TODO

funcClockUse :: EntUse
funcClockUse =
  GenericEntUse "func_clock_use" $ \_ _ _ -> do
    io (putStrLn "GameMisc.funcClockUse") >> undefined -- TODO

spMiscTeleporter :: EdictReference -> Quake ()
spMiscTeleporter _ = io (putStrLn "GameMisc.spMiscTeleporter") >> undefined -- TODO

{-
- QUAKED func_areaportal (0 0 0) ?
- 
- This is a non-visible object that divides the world into areas that are
- seperated when this portal is not activated. Usually enclosed in the
- middle of a door.
-}
spFuncAreaPortal :: EntThink
spFuncAreaPortal =
  GenericEntThink "sp_func_areaportal" $ \(EdictReference edictIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eUse .= Just useAreaPortal
      eCount .= 0 -- always start closed

    return True

{-
- QUAKED misc_teleporter_dest (1 0 0) (-32 -32 -24) (32 32 -16) Point
- teleporters at these.
-}
spMiscTeleporterDest :: EntThink
spMiscTeleporterDest =
  GenericEntThink "SP_misc_teleporter_dest" $ \er@(EdictReference edictIdx) -> do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    setModel er (Just "models/objects/dmspot/tris.md2")

    zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
      eEntityState.esSkinNum .= 0
      eSolid .= Constants.solidBbox
      eMins .= V3 (-32) (-32) (-24)
      eMaxs .= V3 32 32 (-16)

    linkEntity er
    return True

miscDeadSoldierDie :: EntDie
miscDeadSoldierDie =
  GenericEntDie "misc_deadsoldier_die" $ \_ _ _ _ _ -> do
    io (putStrLn "GameMisc.miscDeadSoldierDie") >> undefined -- TODO

throwGib :: EdictReference -> B.ByteString -> Int -> Int -> Quake ()
throwGib _ _ _ _ = io (putStrLn "GameMisc.throwGib") >> undefined -- TODO

throwHead :: EdictReference -> B.ByteString -> Int -> Int -> Quake ()
throwHead _ _ _ _ = io (putStrLn "GameMisc.throwHead") >> undefined -- TODO

barrelDelay :: EntDie
barrelDelay =
  GenericEntDie "barrel_delay" $ \selfRef@(EdictReference selfIdx) attackerRef _ _ _ -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eTakeDamage .= Constants.damageNo
      eNextThink .= levelTime + 2 * Constants.frameTime
      eThink .= Just barrelExplode
      eActivator .= Just attackerRef

barrelExplode :: EntThink
barrelExplode =
  GenericEntThink "barrel_explode" $ \selfRef@(EdictReference selfIdx) -> do
    preuse (gameBaseGlobals.gbGEdicts.ix selfIdx) >>= \(Just self) -> do
      GameCombat.radiusDamage selfRef (fromJust $ self^.eActivator) (fromIntegral $ self^.eDmg) Nothing (fromIntegral (self^.eDmg) + 40) Constants.modBarrel

    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    let save = self^.eEntityState.esOrigin
        origin = (self^.eAbsMin) + fmap (* 0.5) (self^.eSize)

    -- a few big chunks
    let spd = 1.5 * fromIntegral (self^.eDmg) / 200
    gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esOrigin .= origin
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

    gameBaseGlobals.gbGEdicts.ix selfIdx.eEntityState.esOrigin .= save

    preuse (gameBaseGlobals.gbGEdicts.ix selfIdx) >>= \(Just self) ->
      case self^.eGroundEntity of
        Nothing -> becomeExplosion1 selfRef
        Just _ -> becomeExplosion2 selfRef

    return True

  where bigChunksDamage :: EdictReference -> Float -> V3 Float -> Quake ()
        bigChunksDamage selfRef@(EdictReference selfIdx) spd origin = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

          r1 <- Lib.crandom
          r2 <- Lib.crandom
          r3 <- Lib.crandom

          let a = (origin^._x) + r1 * (self^.eSize._x)
              b = (origin^._y) + r2 * (self^.eSize._y)
              c = (origin^._z) + r3 * (self^.eSize._z)
              org = V3 a b c

          throwDebris selfRef "models/objects/debris1/tris.md2" spd org

        littleChunksDamage :: EdictReference -> Float -> V3 Float -> Quake ()
        littleChunksDamage selfRef@(EdictReference selfIdx) spd origin = do
          Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

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
  GenericEntTouch "barrel_touch" $ \selfRef@(EdictReference selfIdx) otherRef@(EdictReference otherIdx) _ _ -> do
    Just other <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx

    unless (isNothing (other^.eGroundEntity) || (other^.eGroundEntity) == Just selfRef) $ do
      Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

      let ratio = fromIntegral (other^.eMass) / fromIntegral (self^.eMass)
          v = (self^.eEntityState.esOrigin) - (other^.eEntityState.esOrigin)

      void $ M.walkMove selfRef (Math3D.vectorYaw v) (20 * ratio * Constants.frameTime)

gibDie :: EntDie
gibDie =
  GenericEntDie "gib_die" $ \_ _ _ _ _ -> do
    io (putStrLn "GameMisc.gibDie") >> undefined -- TODO

useAreaPortal :: EntUse
useAreaPortal =
  GenericEntUse "use_areaportal" $ \_ _ _ -> do
    io (putStrLn "GameMisc.useAreaPortal") >> undefined -- TODO

funcExplosiveUse :: EntUse
funcExplosiveUse =
  FuncExplosiveUse "func_explosive_use" $ \_ _ _ -> do
    io (putStrLn "GameMisc.funcExplosiveUse") >> undefined -- TODO

funcExplosiveSpawn :: EntUse
funcExplosiveSpawn =
  GenericEntUse "func_explosive_spawn" $ \_ _ _ -> do
    io (putStrLn "GameMisc.funcExplosiveSpawn") >> undefined -- TODO

funcExplosiveExplode :: EntDie
funcExplosiveExplode =
  GenericEntDie "func_explosive_explode" $ \_ _ _ _ _ -> do
    io (putStrLn "GameMisc.funcExplosiveExplode") >> undefined -- TODO

pointCombatTouch :: EntTouch
pointCombatTouch =
  GenericEntTouch "point_combat_touch" $ \selfRef@(EdictReference selfIdx) otherRef@(EdictReference otherIdx) plane surf -> do
    Just other <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx

    when ((other^.eMoveTarget) == Just selfRef) $ do
      Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

      if | isJust (self^.eTarget) -> do
             target <- GameBase.pickTarget (self^.eTarget)

             zoom (gameBaseGlobals.gbGEdicts.ix otherIdx) $ do
               eTarget .= (self^.eTarget)
               eGoalEntity .= target
               eMoveTarget .= target

             when (isNothing target) $ do
               dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
               dprintf ((self^.eClassName) `B.append` " at " `B.append` Lib.vtos (self^.eEntityState.esOrigin) `B.append` " target " `B.append` (fromJust $ self^.eTarget) `B.append` " does not exist\n")
               gameBaseGlobals.gbGEdicts.ix otherIdx.eMoveTarget .= Just selfRef

             gameBaseGlobals.gbGEdicts.ix selfIdx.eTarget .= Nothing

         | (self^.eSpawnFlags) .&. 1 /= 0 && (other^.eFlags) .&. (Constants.flSwim .|. Constants.flFly) == 0 -> do
             levelTime <- use $ gameBaseGlobals.gbLevel.llTime

             zoom (gameBaseGlobals.gbGEdicts.ix otherIdx.eMonsterInfo) $ do
               miPauseTime .= levelTime + 100000000
               miAIFlags %= (.|. Constants.aiStandGround)
            
             void $ think (fromJust $ other^.eMonsterInfo.miStand) otherRef

         | otherwise ->
             return ()

      Just other' <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx

      when ((other'^.eMoveTarget) == Just selfRef) $ do
        zoom (gameBaseGlobals.gbGEdicts.ix otherIdx) $ do
          eTarget .= Nothing
          eMoveTarget .= Nothing
          eGoalEntity .= (other'^.eEnemy)
          eMonsterInfo.miAIFlags %= (.&. (complement Constants.aiCombatPoint))
      when (isJust (self^.ePathTarget)) $ do
        let saveTarget = self^.eTarget

        gameBaseGlobals.gbGEdicts.ix selfIdx.eTarget .= (self^.ePathTarget)

        enemyClient <- case other'^.eEnemy of
                         Nothing -> return Nothing
                         Just (EdictReference enemyIdx) -> do
                           Just enemy <- preuse $ gameBaseGlobals.gbGEdicts.ix enemyIdx
                           return (enemy^.eClient)

        oldEnemyClient <- case other'^.eOldEnemy of
                            Nothing -> return Nothing
                            Just (EdictReference oldEnemyIdx) -> do
                              Just oldEnemy <- preuse $ gameBaseGlobals.gbGEdicts.ix oldEnemyIdx
                              return (oldEnemy^.eClient)

        activatorClient <- case other'^.eActivator of
                             Nothing -> return Nothing
                             Just (EdictReference activatorIdx) -> do
                               Just activator <- preuse $ gameBaseGlobals.gbGEdicts.ix activatorIdx
                               return (activator^.eClient)

        let activator = if | isJust enemyClient -> (other'^.eEnemy)
                           | isJust oldEnemyClient -> (other'^.eOldEnemy)
                           | isJust activatorClient -> (other'^.eActivator)
                           | otherwise -> Just otherRef

        GameUtil.useTargets selfRef activator
        gameBaseGlobals.gbGEdicts.ix selfIdx.eTarget .= saveTarget

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
  GenericEntUse "misc_strogg_ship_use" $ \selfRef@(EdictReference selfIdx) otherRef activatorRef -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eSvFlags %= (.&. (complement Constants.svfNoClient))
      eUse .= Just GameFunc.trainUse

    entUse GameFunc.trainUse selfRef otherRef activatorRef

funcWallUse :: EntUse
funcWallUse =
  GenericEntUse "func_wall_use" $ \_ _ _ -> do
    io (putStrLn "GameMisc.funcWallUse") >> undefined -- TODO

{-
- QUAKED misc_banner (1 .5 0) (-4 -4 -4) (4 4 4) The origin is the bottom
- of the banner. The banner is 128 tall.
-}
miscBannerThink :: EntThink
miscBannerThink =
  GenericEntThink "misc_banner_think" $ \(EdictReference edictIdx) -> do
    levelTime <- use $ gameBaseGlobals.gbLevel.llTime
    gameBaseGlobals.gbGEdicts.ix edictIdx.eEntityState.esFrame %= (`mod` 16) . (+ 1)
    gameBaseGlobals.gbGEdicts.ix edictIdx.eNextThink .= levelTime + Constants.frameTime
    return True

funcObjectRelease :: EntThink
funcObjectRelease =
  GenericEntThink "func_object_release" $ \(EdictReference selfIdx) -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eMoveType .= Constants.moveTypeToss
      eTouch .= Just funcObjectTouch

    return True

funcObjectUse :: EntUse
funcObjectUse =
  GenericEntUse "func_object_use" $ \selfRef@(EdictReference selfIdx) _ _ -> do
    zoom (gameBaseGlobals.gbGEdicts.ix selfIdx) $ do
      eSolid .= Constants.solidBsp
      eSvFlags %= (.&. (complement Constants.svfNoClient))
      eUse .= Nothing

    GameUtil.killBox selfRef
    void $ think funcObjectRelease selfRef

{-
- QUAKED func_object (0 .5 .8) ? TRIGGER_SPAWN ANIMATED ANIMATED_FAST This
- is solid bmodel that will fall if it's support it removed.
-}
funcObjectTouch :: EntTouch
funcObjectTouch =
  GenericEntTouch "func_object_touch" $ \selfRef@(EdictReference selfIdx) otherRef@(EdictReference otherIdx) plane _ -> do
    -- TODO: jake2 checks if (plane == null)
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
    Just other <- preuse $ gameBaseGlobals.gbGEdicts.ix otherIdx

    unless ((plane^.cpNormal._z) < 1.0 || (other^.eTakeDamage) == Constants.damageNo) $ do
      v3o <- use $ globals.vec3Origin
      GameCombat.damage otherRef selfRef selfRef v3o (self^.eEntityState.esOrigin) v3o (self^.eDmg) 1 0 Constants.modCrush

miscBlackHoleUse :: EntUse
miscBlackHoleUse =
  GenericEntUse "misc_blackhole_use" $ \_ _ _ -> do
    io (putStrLn "GameMisc.miscBlackHoleUse") >> undefined -- TODO

miscBlackHoleThink :: EntThink
miscBlackHoleThink =
  GenericEntThink "misc_blackhole_think" $ \_ -> do
    io (putStrLn "GameMisc.miscBlackHoleThink") >> undefined -- TODO

miscEasterTankThink :: EntThink
miscEasterTankThink =
  GenericEntThink " misc_eastertank_think" $ \_ -> do
    io (putStrLn "GameMisc.miscEasterTankThink") >> undefined -- TODO

miscEasterChickThink :: EntThink
miscEasterChickThink =
  GenericEntThink "misc_easterchick_think" $ \_ -> do
    io (putStrLn "GameMisc.miscEasterChickThink") >> undefined -- TODO

miscEasterChick2Think :: EntThink
miscEasterChick2Think =
  GenericEntThink "misc_easterchick2_think" $ \_ -> do
    io (putStrLn "GameMisc.miscEasterChick2Think") >> undefined -- TODO

commanderBodyUse :: EntUse
commanderBodyUse =
  GenericEntUse "commander_body_use" $ \_ _ _ -> do
    io (putStrLn "GameMisc.commanderBodyUse") >> undefined -- TODO

commanderBodyDrop :: EntThink
commanderBodyDrop =
  GenericEntThink "commander_body_drop" $ \_ -> do
    io (putStrLn "GameMisc.commanderBodyDrop") >> undefined -- TODO

throwDebris :: EdictReference -> B.ByteString -> Float -> V3 Float -> Quake ()
throwDebris selfRef@(EdictReference selfIdx) modelName speed origin = do
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    chunkRef@(EdictReference chunkIdx) <- GameUtil.spawn
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx

    c1 <- Lib.crandom
    c2 <- Lib.crandom
    c3 <- Lib.crandom

    let v = V3 (100 * c1) (100 * c2) (100 + 100 * c3)

    r1 <- Lib.randomF
    r2 <- Lib.randomF
    r3 <- Lib.randomF

    let avelocity = V3 (r1 * 600) (r2 * 600) (r3 * 600)

    r <- Lib.randomF

    levelTime <- use $ gameBaseGlobals.gbLevel.llTime

    zoom (gameBaseGlobals.gbGEdicts.ix chunkIdx) $ do
      eEntityState.esOrigin .= origin
      eVelocity .= (self^.eVelocity) + fmap (* speed) v
      eMoveType .= Constants.moveTypeBounce
      eSolid .= Constants.solidNot
      eAVelocity .= avelocity
      eThink .= Just GameUtil.freeEdictA
      eNextThink .= levelTime + 5 + r * 5
      eEntityState.esFrame .= 0
      eFlags .= 0
      eClassName .= "debris"
      eTakeDamage .= Constants.damageYes
      eDie .= Just debrisDie

    setModel chunkRef (Just modelName)

    linkEntity chunkRef

becomeExplosion1 :: EdictReference -> Quake ()
becomeExplosion1 selfRef@(EdictReference selfIdx) = do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
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
becomeExplosion2 selfRef@(EdictReference selfIdx) = do
    Just self <- preuse $ gameBaseGlobals.gbGEdicts.ix selfIdx
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
