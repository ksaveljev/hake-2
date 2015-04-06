{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameMisc where

import Control.Lens (use, preuse, (^.), ix, (.=), zoom, (%=))
import Control.Monad (liftM, when)
import Data.Bits ((.|.), (.&.))
import Data.Maybe (isNothing)
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import qualified Constants
import qualified Game.GameUtil as GameUtil

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
  GenericEntUse "light_use" $ \_ _ _ -> do
    io (putStrLn "GameMisc.lightUse") >> undefined -- TODO

spPathCorner :: EdictReference -> Quake ()
spPathCorner er@(EdictReference edictIdx) = do
    gameImport <- use $ gameBaseGlobals.gbGameImport
    let dprintf = gameImport^.giDprintf
        linkEntity = gameImport^.giLinkEntity

    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    if isNothing (edict^.eEdictInfo.eiTargetName)
      then do
        dprintf $ "path_corner with no targetname at " `B.append` BC.pack (show (edict^.eEntityState.esOrigin)) `B.append` "\n"
        GameUtil.freeEdict er
      else do
        zoom (gameBaseGlobals.gbGEdicts.ix edictIdx) $ do
          eSolid .= Constants.solidTrigger
          eEdictAction.eaTouch .= Just pathCornerTouch
          eEdictMinMax.eMins .= V3 (-8) (-8) (-8)
          eEdictMinMax.eMaxs .= V3 8 8 8
          eSvFlags %= (.|. Constants.svfNoClient)

        linkEntity er

{-
- QUAKED path_corner (.5 .3 0) (-8 -8 -8) (8 8 8) TELEPORT Target: next
- path corner Pathtarget: gets used when an entity that has this
- path_corner targeted touches it
-}
pathCornerTouch :: EntTouch
pathCornerTouch =
  GenericEntTouch "path_corner_touch" $ \_ _ _ _ -> do
    io (putStrLn "GameMisc.pathCornerTouch") >> undefined -- TODO

spPointCombat :: EdictReference -> Quake ()
spPointCombat _ = io (putStrLn "GameMisc.spPointCombat") >> undefined -- TODO

spViewThing :: EdictReference -> Quake ()
spViewThing _ = io (putStrLn "GameMisc.spViewThing") >> undefined -- TODO

spInfoNull :: EdictReference -> Quake ()
spInfoNull _ = io (putStrLn "GameMisc.spInfoNull") >> undefined -- TODO

spInfoNotNull :: EdictReference -> Quake ()
spInfoNotNull _ = io (putStrLn "GameMisc.spInfoNotNull") >> undefined -- TODO

spLight :: EdictReference -> Quake ()
spLight er@(EdictReference edictIdx) = do
    -- no targeted lights in deathmatch, because they cause global messages
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if isNothing (edict^.eEdictInfo.eiTargetName) || deathmatchValue /= 0
      then GameUtil.freeEdict er
      else 
        when ((edict^.eStyle) >= 32) $ do
          gameBaseGlobals.gbGEdicts.ix edictIdx.eEdictAction.eaUse .= Just lightUse

          configString <- use $ gameBaseGlobals.gbGameImport.giConfigString

          if (edict^.eSpawnFlags) .&. startOff /= 0
            then configString (Constants.csLights + (edict^.eStyle)) "a"
            else configString (Constants.csLights + (edict^.eStyle)) "m"

spFuncWall :: EdictReference -> Quake ()
spFuncWall _ = io (putStrLn "GameMisc.spFuncWall") >> undefined -- TODO

spFuncObject :: EdictReference -> Quake ()
spFuncObject _ = io (putStrLn "GameMisc.spFuncObject") >> undefined -- TODO

spFuncExplosive :: EdictReference -> Quake ()
spFuncExplosive _ = io (putStrLn "GameMisc.spFuncExplosive") >> undefined -- TODO

spMiscExploBox :: EdictReference -> Quake ()
spMiscExploBox _ = io (putStrLn "GameMisc.spMiscExploBox") >> undefined -- TODO

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
spMiscBanner _ = io (putStrLn "GameMisc.spMiscBanner") >> undefined -- TODO

spMiscDeadSoldier :: EdictReference -> Quake ()
spMiscDeadSoldier er@(EdictReference edictIdx) = do
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0 -- auto-remove for deathmatch
      then GameUtil.freeEdict er
      else do
        gameImport <- use $ gameBaseGlobals.gbGameImport
        let modelIndex = gameImport^.giModelIndex
            linkEntity = gameImport^.giLinkEntity

        tris <- modelIndex "models/deadbods/dude/tris.md2"

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
          eEdictMinMax.eMins .= V3 (-16) (-16) 0
          eEdictMinMax.eMaxs .= V3 16 16 16
          eEdictStatus.eDeadFlag .= Constants.deadDead
          eEdictStatus.eTakeDamage .= Constants.damageYes
          eSvFlags %= (.|. (Constants.svfMonster .|. Constants.svfDeadMonster))
          eEdictAction.eaDie .= Just miscDeadSoldierDie
          eMonsterInfo.miAIFlags %= (.|. Constants.aiGoodGuy)

        linkEntity er

spMiscViper :: EdictReference -> Quake ()
spMiscViper _ = io (putStrLn "GameMisc.spMiscViper") >> undefined -- TODO

spMiscBigViper :: EdictReference -> Quake ()
spMiscBigViper _ = io (putStrLn "GameMisc.spMiscBigViper") >> undefined -- TODO

spMiscViperBomb :: EdictReference -> Quake ()
spMiscViperBomb _ = io (putStrLn "GameMisc.spMiscViperBomb") >> undefined -- TODO

spMiscStroggShip :: EdictReference -> Quake ()
spMiscStroggShip _ = io (putStrLn "GameMisc.spMiscStroggShip") >> undefined -- TODO

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

spMiscGibHead :: EdictReference -> Quake ()
spMiscGibHead _ = io (putStrLn "GameMisc.spMiscGibHead") >> undefined -- TODO

spTargetCharacter :: EdictReference -> Quake ()
spTargetCharacter _ = io (putStrLn "GameMisc.spTargetCharacter") >> undefined -- TODO

spTargetString :: EdictReference -> Quake ()
spTargetString _ = io (putStrLn "GameMisc.spTargetString") >> undefined -- TODO

spFuncClock :: EdictReference -> Quake ()
spFuncClock _ = io (putStrLn "GameMisc.spFuncClock") >> undefined -- TODO

spMiscTeleporter :: EdictReference -> Quake ()
spMiscTeleporter _ = io (putStrLn "GameMisc.spMiscTeleporter") >> undefined -- TODO

spFuncAreaPortal :: EntThink
spFuncAreaPortal =
  GenericEntThink "sp_func_areaportal" $ \_ -> do
    io (putStrLn "GameMisc.spFuncAreaPortal") >> undefined -- TODO

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
      eEdictMinMax.eMins .= V3 (-32) (-32) (-24)
      eEdictMinMax.eMaxs .= V3 32 32 (-16)

    linkEntity er
    return True

miscDeadSoldierDie :: EntDie
miscDeadSoldierDie =
  GenericEntDie "misc_deadsoldier_die" $ \_ _ _ _ _ -> do
    io (putStrLn "GameMisc.miscDeadSoldierDie") >> undefined -- TODO
