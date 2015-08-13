{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameMisc where

import Control.Lens (use, preuse, (^.), ix, (.=), zoom, (%=), (&), (+~))
import Control.Monad (liftM, when, void, unless)
import Data.Bits ((.|.), (.&.), complement)
import Data.Maybe (isNothing, isJust, fromJust)
import Linear (V3(..), _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Client.M as M
import {-# SOURCE #-} qualified Game.GameBase as GameBase
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
spFuncObject _ = io (putStrLn "GameMisc.spFuncObject") >> undefined -- TODO

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
spMiscBlackHole _ = io (putStrLn "GameMisc.spMiscBlackHole") >> undefined -- TODO

spMiscEasterTank :: EdictReference -> Quake ()
spMiscEasterTank _ = io (putStrLn "GameMisc.spMiscEasterTank") >> undefined -- TODO

spMiscEasterChick :: EdictReference -> Quake ()
spMiscEasterChick _ = io (putStrLn "GameMisc.spMiscEasterChick") >> undefined -- TODO

spMiscEasterChick2 :: EdictReference -> Quake ()
spMiscEasterChick2 _ = io (putStrLn "GameMisc.spMiscEasterChick2") >> undefined -- TODO

spMonsterCommanderBody :: EdictReference -> Quake ()
spMonsterCommanderBody _ = io (putStrLn "GameMisc.spMonsterCommanderBody") >> undefined -- TODO

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
spMiscViper _ = io (putStrLn "GameMisc.spMiscViper") >> undefined -- TODO

spMiscBigViper :: EdictReference -> Quake ()
spMiscBigViper _ = io (putStrLn "GameMisc.spMiscBigViper") >> undefined -- TODO

spMiscViperBomb :: EdictReference -> Quake ()
spMiscViperBomb _ = io (putStrLn "GameMisc.spMiscViperBomb") >> undefined -- TODO

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
spMiscSatelliteDish _ = io (putStrLn "GameMisc.spMiscSatelliteDish") >> undefined -- TODO

spLightMine1 :: EdictReference -> Quake ()
spLightMine1 _ = io (putStrLn "GameMisc.spLightMine1") >> undefined -- TODO

spLightMine2 :: EdictReference -> Quake ()
spLightMine2 _ = io (putStrLn "GameMisc.spLightMine2") >> undefined -- TODO

spMiscGibArm :: EdictReference -> Quake ()
spMiscGibArm _ = io (putStrLn "GameMisc.spMiscGibArm") >> undefined -- TODO

spMiscGibLeg :: EdictReference -> Quake ()
spMiscGibLeg _ = io (putStrLn "GameMisc.spMiscGibLeg") >> undefined -- TODO

{-
- QUAKED misc_gib_head (1 0 0) (-8 -8 -8) (8 8 8) Intended for use with the
- target_spawner
-}
spMiscGibHead :: EdictReference -> Quake ()
spMiscGibHead er@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let setModel = gameImport^.giSetModel
        linkEntity = gameImport^.giLinkEntity

    setModel er (Just "models/objects/gibs/head/tris.md2")

    -- IMPROVE? :)
    r1 <- Lib.randomF
    r2 <- Lib.randomF
    r3 <- Lib.randomF

    time <- use $ gameBaseGlobals.gbLevel.llTime
    
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
      eNextThink .= time + 30

    linkEntity er

spTargetCharacter :: EdictReference -> Quake ()
spTargetCharacter _ = io (putStrLn "GameMisc.spTargetCharacter") >> undefined -- TODO

spTargetString :: EdictReference -> Quake ()
spTargetString _ = io (putStrLn "GameMisc.spTargetString") >> undefined -- TODO

spFuncClock :: EdictReference -> Quake ()
spFuncClock _ = io (putStrLn "GameMisc.spFuncClock") >> undefined -- TODO

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
  GenericEntDie "barrel_delay" $ \_ _ _ _ _ -> do
    io (putStrLn "GameMisc.barrelDelay") >> undefined -- TODO

barrelTouch :: EntTouch
barrelTouch =
  GenericEntTouch "barrel_touch" $ \_ _ _ _ -> do
    io (putStrLn "GameMisc.barrelTouch") >> undefined -- TODO

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
  GenericEntTouch "point_combat_touch" $ \_ _ _ _ -> do
    io (putStrLn "GameMisc.pointCombatTouch") >> undefined -- TODO

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
