{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameSpawn where

import Control.Lens (use, (^.), (.=), (%=), preuse, ix, (&), (.~), (%~))
import Control.Monad (liftM, when, void, unless)
import Data.Bits ((.&.), complement, (.|.))
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, isNothing)
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import Game.SpawnT
import qualified Constants
import qualified Game.GameFunc as GameFunc
import qualified Game.GameItems as GameItems
import qualified Game.GameMisc as GameMisc
import {-# SOURCE #-} qualified Game.GameTarget as GameTarget
import qualified Game.GameTrigger as GameTrigger
import qualified Game.GameTurret as GameTurret
import qualified Game.GameUtil as GameUtil
import qualified Game.Monsters.MActor as MActor
import qualified Game.Monsters.MBerserk as MBerserk
import qualified Game.Monsters.MBoss2 as MBoss2
import qualified Game.Monsters.MBoss3 as MBoss3
import qualified Game.Monsters.MBoss31 as MBoss31
import qualified Game.Monsters.MBrain as MBrain
import qualified Game.Monsters.MChick as MChick
import qualified Game.Monsters.MFlipper as MFlipper
import qualified Game.Monsters.MFloat as MFloat
import qualified Game.Monsters.MFlyer as MFlyer
import qualified Game.Monsters.MGladiator as MGladiator
import qualified Game.Monsters.MGunner as MGunner
import qualified Game.Monsters.MHover as MHover
import qualified Game.Monsters.MInfantry as MInfantry
import qualified Game.Monsters.MInsane as MInsane
import {-# SOURCE #-} qualified Game.Monsters.MMedic as MMedic
import qualified Game.Monsters.MMutant as MMutant
import qualified Game.Monsters.MParasite as MParasite
import qualified Game.Monsters.MSoldier as MSoldier
import qualified Game.Monsters.MSuperTank as MSuperTank
import qualified Game.Monsters.MTank as MTank
import qualified Game.PlayerClient as PlayerClient
import qualified Game.PlayerTrail as PlayerTrail
import qualified QCommon.Com as Com
import qualified Util.Lib as Lib

{-
- SpawnEntities
-
- Creates a server's entity / program execution context by parsing textual
- entity definitions out of an ent file.
-}
spawnEntities :: B.ByteString -> B.ByteString -> B.ByteString -> Quake ()
spawnEntities mapName entities spawnPoint = do
    Com.dprintf $ "SpawnEntities(), mapname=" `B.append` mapName `B.append` "\n"

    -- avoiding the "defaulting the following constraints" warning is fun
    skillValue :: Float <- liftM (fromIntegral . ((floor . (^.cvValue)) :: CVarT -> Int)) skillCVar
    let skillLevel = if | skillValue < 0 -> 0
                        | skillValue > 3 -> 3
                        | otherwise -> skillValue

    when (skillValue /= skillLevel) $ do
      forceSet <- use $ gameBaseGlobals.gbGameImport.giCVarForceSet
      void (forceSet "skill" (BC.pack $ show skillLevel))

    PlayerClient.saveClientData

    gameBaseGlobals.gbLevel .= newLevelLocalsT

    maxEntities <- use $ gameBaseGlobals.gbGame.glMaxEntities
    mapM_ (\i -> writeEdictT (newEdictReference i) (newEdictT i)) [0..maxEntities-1]

    gameBaseGlobals.gbLevel.llMapName .= mapName
    gameBaseGlobals.gbGame.glSpawnPoint .= spawnPoint

    -- set client fields on player ents
    maxClients <- use $ gameBaseGlobals.gbGame.glMaxClients
    mapM_ (\i -> modifyEdictT (newEdictReference i) (\v -> v & eClient .~ Just (GClientReference (i - 1)))) [1..maxClients]

    inhibited <- parseEntities True 0 0

    Com.dprintf $ "player skill level:" `B.append` BC.pack (show skillValue) `B.append` "\n" -- IMPROVE
    Com.dprintf $ BC.pack (show inhibited) `B.append` " entities inhibited.\n"

    findTeams
    PlayerTrail.init

  where parseEntities :: Bool -> Int -> Int -> Quake Int
        parseEntities initial idx inhibited
          | idx >= B.length entities = return inhibited -- RESEARCH: make sure this is correct? is this present in jake2?
          | otherwise = do
              (comToken, newIdx) <- Com.parse entities (B.length entities) idx

              case comToken of
                Nothing -> return inhibited
                Just token -> do

                  when (BC.head token /= '{') $ do
                    err <- use $ gameBaseGlobals.gbGameImport.giError
                    err $ "ED_LoadFromFile: found " `B.append` token `B.append` " when expecting {"

                  edictRef <- if initial
                                then return worldRef
                                else GameUtil.spawn

                  updatedIdx <- parseEdict edictRef entities newIdx
                  edict <- readEdictT edictRef

                  Com.dprintf $ "spawning ent[" `B.append` BC.pack (show $ edict^.eIndex) `B.append` -- IMPROVE
                                "], classname=" `B.append` (edict^.eClassName) `B.append`
                                ", flags=" `B.append` BC.pack (show $ edict^.eSpawnFlags) -- IMPROVE: show flags in hex

                  -- yet another map hack
                  when (BC.map toLower mapName == "command" &&
                        BC.map toLower (edict^.eClassName) == "trigger_once" &&
                        isJust (edict^.eiModel) &&
                        BC.map toLower (fromJust (edict^.eiModel)) == "*27") $
                    modifyEdictT edictRef (\v -> v & eSpawnFlags %~ (.&. (complement Constants.spawnFlagNotHard)))

                  -- remove things (except the world) from different skill levels or deathmatch
                  removed <- if edictRef == worldRef
                               then
                                 return False

                               else do
                                 deathmatchValue <- liftM (^.cvValue) deathmatchCVar

                                 freed <- if deathmatchValue /= 0
                                            then
                                              if ((edict^.eSpawnFlags) .&. Constants.spawnFlagNotDeathmatch) /= 0
                                                then do
                                                  Com.dprintf "->inhibited.\n"
                                                  GameUtil.freeEdict edictRef
                                                  return True
                                                else
                                                  return False

                                            else do
                                              skillValue <- liftM (^.cvValue) skillCVar

                                              if (skillValue == 0 && (edict^.eSpawnFlags .&. Constants.spawnFlagNotEasy) /= 0) ||
                                                 (skillValue == 1 && (edict^.eSpawnFlags .&. Constants.spawnFlagNotMedium) /= 0) ||
                                                 ((skillValue == 2 || skillValue == 3) && (edict^.eSpawnFlags .&. Constants.spawnFlagNotHard) /= 0)
                                                 then do
                                                   Com.dprintf "->inhibited.\n"
                                                   GameUtil.freeEdict edictRef
                                                   return True
                                                 else
                                                   return False

                                 if freed
                                   then do
                                     let flags = Constants.spawnFlagNotEasy .|. Constants.spawnFlagNotMedium .|. Constants.spawnFlagNotHard .|. Constants.spawnFlagNotCoop .|. Constants.spawnFlagNotDeathmatch
                                     modifyEdictT edictRef (\v -> v & eSpawnFlags %~ (.&. (complement flags)))
                                     return True
                                   else do
                                     return False


                  if removed
                    then
                      parseEntities False updatedIdx (inhibited + 1)

                    else do
                      callSpawn edictRef
                      Com.dprintf "\n"

                      parseEntities False updatedIdx inhibited

{-
- ED_ParseEdict
-
- Parses an edict out of the given string, returning the new position ed
- should be a properly initialized empty edict.
-}
parseEdict :: EdictReference -> B.ByteString -> Int -> Quake Int
parseEdict edictRef entities idx = do
    gameBaseGlobals.gbSpawnTemp .= newSpawnTempT

    (newIdx, initial) <- parse False idx

    unless initial $ GameUtil.clearEdict edictRef

    return newIdx

  where parse :: Bool -> Int -> Quake (Int, Bool)
        parse initial index = do
          (comToken, newIdx) <- Com.parse entities (B.length entities) index

          case comToken of
            Nothing -> do
              err <- use $ gameBaseGlobals.gbGameImport.giError
              err "ED_ParseEntity: EOF without closing brace"
              return (newIdx, initial)

            Just token -> do
              if token == "}"
                then
                  return (newIdx, initial)

                else do
                  let keyName = token
                  (anotherComToken, finalIdx) <- Com.parse entities (B.length entities) newIdx

                  case anotherComToken of
                    Nothing -> do
                      err <- use $ gameBaseGlobals.gbGameImport.giError
                      err "ED_ParseEntity: EOF without closing brace"
                      return (finalIdx, initial)

                    Just anotherToken -> do
                      when (anotherToken == "}") $ do
                        err <- use $ gameBaseGlobals.gbGameImport.giError
                        err "ED_ParseEntity: closing brace without data"

                      -- keynames with a leading underscore are used for utility comments,
                      -- and are immediately discarded by quake
                      if BC.head keyName == '_'
                        then
                          parse True finalIdx

                        else do
                          parseField (BC.map toLower keyName) anotherToken edictRef
                          parse True finalIdx

parseField :: B.ByteString -> B.ByteString -> EdictReference -> Quake ()
parseField key value edictRef = do
    when (key == "nextmap") $
      Com.println $ "nextmap: " `B.append` value

    st <- use $ gameBaseGlobals.gbSpawnTemp
    let (updatedSt, didUpdateSpawnTemp) = setSpawnTempField st key value

    if didUpdateSpawnTemp
      then
        gameBaseGlobals.gbSpawnTemp .= updatedSt

      else do
        edict <- readEdictT edictRef
        let (updatedEdict, didUpdateEdict) = setEdictField edict key value
        if didUpdateEdict
          then
            writeEdictT edictRef updatedEdict
          else do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf $ "??? The key [" `B.append` key `B.append` "] is not a field\n"

setSpawnTempField :: SpawnTempT -> B.ByteString -> B.ByteString -> (SpawnTempT, Bool)
setSpawnTempField st key value =
    case key of
      "lip"       -> (st { _stLip       = Lib.atoi         value }, True)
      "distance"  -> (st { _stDistance  = Lib.atoi         value }, True)
      "height"    -> (st { _stHeight    = Lib.atoi         value }, True)
      "noise"     -> (st { _stNoise     = Just $ newString value }, True)
      "pausetime" -> (st { _stPauseTime = Lib.atof         value }, True)
      "item"      -> (st { _stItem      = Just $ newString value }, True)
      "gravity"   -> (st { _stGravity   = Just $ newString value }, True)
      "sky"       -> (st { _stSky       = newString        value }, True)
      "skyrotate" -> (st { _stSkyRotate = Lib.atof         value }, True)
      "skyaxis"   -> (st { _stSkyAxis   = Lib.atov         value }, True)
      "minyaw"    -> (st { _stMinYaw    = Lib.atof         value }, True)
      "maxyaw"    -> (st { _stMaxYaw    = Lib.atof         value }, True)
      "minpitch"  -> (st { _stMinPitch  = Lib.atof         value }, True)
      "maxpitch"  -> (st { _stMaxPitch  = Lib.atof         value }, True)
      "nextmap"   -> (st { _stNextMap   = newString        value }, True)
      _           -> (st, False)

setEdictField :: EdictT -> B.ByteString -> B.ByteString -> (EdictT, Bool)
setEdictField e key value =
    case key of
      "classname"    -> (e { _eClassName = newString value }, True)
      "model"        -> (e { _eiModel = Just (newString value) }, True)
      "spawnflags"   -> (e { _eSpawnFlags = Lib.atoi value }, True)
      "speed"        -> (e { _eSpeed = Lib.atof value }, True )
      "accel"        -> (e { _eAccel = Lib.atof value }, True )
      "decel"        -> (e { _eDecel = Lib.atof value }, True )
      "target"       -> (e { _eTarget = Just (newString value) }, True)
      "targetname"   -> (e { _eTargetName = Just (newString value) }, True)
      "pathtarget"   -> (e { _ePathTarget = Just (newString value) }, True)
      "deathtarget"  -> (e { _eDeathTarget = Just (newString value) }, True)
      "killtarget"   -> (e { _eKillTarget = Just (newString value) }, True)
      "combattarget" -> (e { _eCombatTarget = Just (newString value) }, True)
      "message"      -> (e { _eMessage = Just (newString value) }, True)
      "team"         -> (e { _eTeam = Just (newString value) }, True) -- TODO: we need to call Com.dprintf here
      "wait"         -> (e { _eWait = Lib.atof value }, True)
      "delay"        -> (e { _eDelay = Lib.atof value }, True)
      "random"       -> (e { _eRandom = Lib.atof value }, True)
      "move_origin"  -> (e { _eMoveOrigin = Lib.atov value }, True)
      "move_angles"  -> (e { _eMoveAngles = Lib.atov value }, True)
      "style"        -> (e { _eStyle = Lib.atoi value }, True)
      "count"        -> (e { _eCount = Lib.atoi value }, True)
      "health"       -> (e { _eHealth = Lib.atoi value }, True)
      "sounds"       -> (e { _eSounds = Lib.atoi value }, True)
      "light"        -> (e, True)
      "dmg"          -> (e { _eDmg = Lib.atoi value }, True)
      "mass"         -> (e { _eMass = Lib.atoi value }, True)
      "volume"       -> (e { _eVolume = Lib.atof value }, True)
      "attenuation"  -> (e { _eAttenuation = Lib.atof value }, True)
      "map"          -> (e { _eMap = Just (newString value) }, True)
      "origin"       -> (e { _eEntityState = (e^.eEntityState) { _esOrigin = Lib.atov value } }, True)
      "angles"       -> (e { _eEntityState = (e^.eEntityState) { _esAngles = Lib.atov value } }, True)
      "angle"        -> (e { _eEntityState = (e^.eEntityState) { _esAngles = V3 0 (Lib.atof value) 0 } }, True)
      "item"         -> (e, True) -- TODO: we need to call error here!!!
      _              -> (e, False)

newString :: B.ByteString -> B.ByteString
newString str = let len = B.length str
                in composeString 0 len ""

  where composeString :: Int -> Int -> B.ByteString -> B.ByteString
        composeString idx len acc
          | idx == len = acc
          | otherwise =
              let c = BC.index str idx
              in if c == '\\' && idx < len - 1
                   then let c' = BC.index str (idx + 1)
                        in if c' == 'n'
                             then composeString (idx + 2) len (acc `BC.snoc` '\n')
                             else composeString (idx + 2) len (acc `BC.snoc` '\\')
                   else composeString (idx + 1) len (acc `BC.snoc` c)

{-
- ED_CallSpawn
-
- Finds the spawn function for the entity and calls it.
-}
callSpawn :: EdictReference -> Quake ()
callSpawn edictRef = do
    numItems <- use $ gameBaseGlobals.gbGame.glNumItems
    edict <- readEdictT edictRef

    -- IMPROVE: does it apply to our code?
    -- if (null == ent.classname) {
    --     GameBase.gi.dprintf("ED_CallSpawn: null classname\n");
    --     return;
    -- }

    -- check item spawn functions
    let edictClassName = BC.map toLower (edict^.eClassName)
    itemSpawnIndex <- checkItemSpawn edictClassName 1 numItems

    case itemSpawnIndex of
      Just gItemReference ->
        GameItems.spawnItem edictRef gItemReference

      Nothing -> do
        -- check normal spawn functions
        let spawnIdx = V.findIndex (\s -> edictClassName == BC.map toLower (s^.spName)) spawns

        case spawnIdx of
          Just idx ->
            void $ think ((spawns V.! idx)^.spSpawn) edictRef

          Nothing -> do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf $ edictClassName `B.append` " doesn't have a spawn function\n"

  where checkItemSpawn :: B.ByteString -> Int -> Int -> Quake (Maybe GItemReference)
        checkItemSpawn edictClassName idx maxIdx
          | idx == maxIdx = return Nothing
          | otherwise = do
              Just item <- preuse $ gameBaseGlobals.gbItemList.ix idx
              if edictClassName == BC.map toLower (item^.giClassName)
                then return (Just $ GItemReference idx)
                else checkItemSpawn edictClassName (idx + 1) maxIdx

{-
- G_FindTeams
- 
- Chain together all entities with a matching team field.
- 
- All but the first will have the FL_TEAMSLAVE flag set. All but the last
- will have the teamchain field set to the next one.
-}
findTeams :: Quake ()
findTeams = do
    numEdicts <- use $ gameBaseGlobals.gbNumEdicts

    (teamsNumber, entitiesNumber) <- findNextTeam numEdicts 1 0 0
    dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
    dprintf $ BC.pack (show teamsNumber) `B.append` " teams with " `B.append` BC.pack (show entitiesNumber) `B.append` " entities\n"

  where findNextTeam :: Int -> Int -> Int -> Int -> Quake (Int, Int)
        findNextTeam maxIdx idx c c2
          | idx >= maxIdx = return (c, c2)
          | otherwise = do
              let edictRef = newEdictReference idx
              edict <- readEdictT edictRef

              if not (edict^.eInUse) || isNothing (edict^.eTeam) || (edict^.eFlags) .&. Constants.flTeamSlave /= 0
                then
                  findNextTeam maxIdx (idx + 1) c c2

                else do
                  modifyEdictT edictRef (\v -> v & eTeamMaster .~ Just edictRef)
                  c2' <- findTeamMembers (fromJust $ edict^.eTeam) edictRef edictRef maxIdx (idx + 1) c2
                  findNextTeam maxIdx (idx + 1) (c + 1) c2'

        findTeamMembers :: B.ByteString -> EdictReference -> EdictReference -> Int -> Int -> Int -> Quake Int
        findTeamMembers teamName master chainRef maxIdx idx c2
          | idx >= maxIdx = return c2
          | otherwise = do
              let edictRef = newEdictReference idx
              edict <- readEdictT edictRef

              if not (edict^.eInUse) || isNothing (edict^.eTeam) || (edict^.eFlags) .&. Constants.flTeamSlave /= 0 || teamName /= fromJust (edict^.eTeam)
                then
                  findTeamMembers teamName master chainRef maxIdx (idx + 1) c2
                else do
                  modifyEdictT chainRef (\v -> v & eTeamChain .~ Just edictRef)
                  modifyEdictT edictRef (\v -> v & eTeamMaster .~ Just master
                                                 & eFlags %~ (.|. Constants.flTeamSlave))
                  findTeamMembers teamName master edictRef maxIdx (idx + 1) (c2 + 1)

spawns :: V.Vector SpawnT
spawns = V.fromList [ SpawnT "item_health" spItemHealth
                    , SpawnT "item_health_small" spItemHealthSmall
                    , SpawnT "item_health_large" spItemHealthLarge
                    , SpawnT "item_health_mega" spItemHealthMega
                    , SpawnT "info_player_start" spInfoPlayerStart
                    , SpawnT "info_player_deathmatch" spInfoPlayerDeathmatch
                    , SpawnT "info_player_coop" spInfoPlayerCoop
                    , SpawnT "info_player_intermission" spInfoPlayerIntermission
                    , SpawnT "func_plat" spFuncPlat
                    , SpawnT "func_button" GameFunc.spFuncButton
                    , SpawnT "func_door" GameFunc.spFuncDoor
                    , SpawnT "func_door_secret" GameFunc.spFuncDoorSecret
                    , SpawnT "func_door_rotating" GameFunc.spFuncDoorRotating
                    , SpawnT "func_rotating" GameFunc.spFuncRotating
                    , SpawnT "func_train" spFuncTrain
                    , SpawnT "func_water" spFuncWater
                    , SpawnT "func_conveyor" GameFunc.spFuncConveyor
                    , SpawnT "func_areaportal" GameMisc.spFuncAreaPortal
                    , SpawnT "func_clock" spFuncClock
                    , SpawnT "func_wall" spFuncWall
                    , SpawnT "func_object" spFuncObject
                    , SpawnT "func_timer" spFuncTimer
                    , SpawnT "func_explosive" spFuncExplosive
                    , SpawnT "func_killbox" GameFunc.spFuncKillBox
                    , SpawnT "trigger_always" spTriggerAlways
                    , SpawnT "trigger_once" spTriggerOnce
                    , SpawnT "trigger_multiple" spTriggerMultiple
                    , SpawnT "trigger_relay" spTriggerRelay
                    , SpawnT "trigger_push" spTriggerPush
                    , SpawnT "trigger_hurt" spTriggerHurt
                    , SpawnT "trigger_key" spTriggerKey
                    , SpawnT "trigger_counter" spTriggerCounter
                    , SpawnT "trigger_elevator" GameFunc.spTriggerElevator
                    , SpawnT "trigger_gravity" spTriggerGravity
                    , SpawnT "trigger_monsterjump" spTriggerMonsterJump
                    , SpawnT "target_temp_entity" spTargetTempEntity
                    , SpawnT "target_speaker" spTargetSpeaker
                    , SpawnT "target_explosion" spTargetExplosion
                    , SpawnT "target_changelevel" spTargetChangeLevel
                    , SpawnT "target_secret" spTargetSecret
                    , SpawnT "target_goal" spTargetGoal
                    , SpawnT "target_splash" spTargetSplash
                    , SpawnT "target_spawner" spTargetSpawner
                    , SpawnT "target_blaster" spTargetBlaster
                    , SpawnT "target_crosslevel_trigger" spTargetCrossLevelTrigger
                    , SpawnT "target_crosslevel_target" spTargetCrossLevelTarget
                    , SpawnT "target_laser" spTargetLaser
                    , SpawnT "target_help" spTargetHelp
                    , SpawnT "target_actor" spTargetActor
                    , SpawnT "target_lightramp" spTargetLightRamp
                    , SpawnT "target_earthquake" spTargetEarthquake
                    , SpawnT "target_character" spTargetCharacter
                    , SpawnT "target_string" spTargetString
                    , SpawnT "worldspawn" spWorldSpawn
                    , SpawnT "viewthing" spViewThing
                    , SpawnT "light" spLight
                    , SpawnT "light_mine1" spLightMine1
                    , SpawnT "light_mine2" spLightMine2
                    , SpawnT "info_null" spInfoNull
                    , SpawnT "func_group" spInfoNull
                    , SpawnT "info_notnull" spInfoNotNull
                    , SpawnT "path_corner" spPathCorner
                    , SpawnT "point_combat" spPointCombat
                    , SpawnT "misc_explobox" spMiscExploBox
                    , SpawnT "misc_banner" spMiscBanner
                    , SpawnT "misc_satellite_dish" spMiscSatelliteDish
                    , SpawnT "misc_actor" spMiscActor
                    , SpawnT "misc_gib_arm" spMiscGibArm
                    , SpawnT "misc_gib_leg" spMiscGibLeg
                    , SpawnT "misc_gib_head" spMiscGibHead
                    , SpawnT "misc_insane" spMiscInsane
                    , SpawnT "misc_deadsoldier" spMiscDeadSoldier
                    , SpawnT "misc_viper" spMiscViper
                    , SpawnT "misc_viper_bomb" spMiscViperBomb
                    , SpawnT "misc_bigviper" spMiscBigViper
                    , SpawnT "misc_strogg_ship" spMiscStroggShip
                    , SpawnT "misc_teleporter" spMiscTeleporter
                    , SpawnT "misc_teleporter_dest" GameMisc.spMiscTeleporterDest
                    , SpawnT "misc_blackhole" spMiscBlackHole
                    , SpawnT "misc_eastertank" spMiscEasterTank
                    , SpawnT "misc_easterchick" spMiscEasterChick
                    , SpawnT "misc_easterchick2" spMiscEasterChick2
                    , SpawnT "monster_berserk" spMonsterBerserk
                    , SpawnT "monster_gladiator" spMonsterGladiator
                    , SpawnT "monster_gunner" spMonsterGunner
                    , SpawnT "monster_infantry" spMonsterInfantry
                    , SpawnT "monster_soldier_light" MSoldier.spMonsterSoldierLight
                    , SpawnT "monster_soldier" MSoldier.spMonsterSoldier
                    , SpawnT "monster_soldier_ss" MSoldier.spMonsterSoldierSS
                    , SpawnT "monster_tank" MTank.spMonsterTank
                    , SpawnT "monster_tank_commander" MTank.spMonsterTank
                    , SpawnT "monster_medic" spMonsterMedic
                    , SpawnT "monster_flipper" spMonsterFlipper
                    , SpawnT "monster_chick" spMonsterChick
                    , SpawnT "monster_parasite" MParasite.spMonsterParasite
                    , SpawnT "monster_flyer" spMonsterFlyer
                    , SpawnT "monster_brain" spMonsterBrain
                    , SpawnT "monster_floater" spMonsterFloater
                    , SpawnT "monster_hover" spMonsterHover
                    , SpawnT "monster_mutant" MMutant.spMonsterMutant
                    , SpawnT "monster_supertank" MSuperTank.spMonsterSuperTank
                    , SpawnT "monster_boss2" spMonsterBoss2
                    , SpawnT "monster_boss3_stand" spMonsterBoss3Stand
                    , SpawnT "monster_jorg" spMonsterJorg
                    , SpawnT "monster_commander_body" spMonsterCommanderBody
                    , SpawnT "turret_breach" spTurretBreach
                    , SpawnT "turret_base" spTurretBase
                    , SpawnT "turret_driver" spTurretDriver
                    ]

spItemHealth :: EntThink
spItemHealth =
  GenericEntThink "SP_item_health" $ \edictReference -> do
    GameItems.spItemHealth edictReference
    return True

spItemHealthSmall :: EntThink
spItemHealthSmall =
  GenericEntThink "SP_item_health_small" $ \edictReference -> do
    GameItems.spItemHealthSmall edictReference
    return True

spItemHealthLarge :: EntThink
spItemHealthLarge =
  GenericEntThink "SP_item_health_large" $ \edictReference -> do
    GameItems.spItemHealthLarge edictReference
    return True

spItemHealthMega :: EntThink
spItemHealthMega =
  GenericEntThink "SP_item_health_mega" $ \edictReference -> do
    GameItems.spItemHealthMega edictReference
    return True

spInfoPlayerStart :: EntThink
spInfoPlayerStart =
  GenericEntThink "SP_info_player_start" $ \edictReference -> do
    PlayerClient.spInfoPlayerStart edictReference
    return True

spInfoPlayerDeathmatch :: EntThink
spInfoPlayerDeathmatch =
  GenericEntThink "SP_info_player_deathmatch" $ \edictReference -> do
    PlayerClient.spInfoPlayerDeathmatch edictReference
    return True

spInfoPlayerCoop :: EntThink
spInfoPlayerCoop =
  GenericEntThink "SP_info_player_coop" $ \edictReference -> do
    PlayerClient.spInfoPlayerCoop edictReference
    return True

spInfoPlayerIntermission :: EntThink
spInfoPlayerIntermission =
  GenericEntThink "SP_info_player_intermission" $ \_ -> do
    PlayerClient.spInfoPlayerIntermission
    return True

spFuncPlat :: EntThink
spFuncPlat =
  GenericEntThink "SP_func_plat" $ \edictReference -> do
    GameFunc.spFuncPlat edictReference
    return True

spFuncWater :: EntThink
spFuncWater =
  GenericEntThink "SP_func_water" $ \edictReference -> do
    GameFunc.spFuncWater edictReference
    return True

spFuncTrain :: EntThink
spFuncTrain =
  GenericEntThink "SP_func_train" $ \edictReference -> do
    GameFunc.spFuncTrain edictReference
    return True

spFuncClock :: EntThink
spFuncClock =
  GenericEntThink "SP_func_clock" $ \edictReference -> do
    GameMisc.spFuncClock edictReference
    return True

{-
- QUAKED worldspawn (0 0 0) ?
-
- Only used for the world. "sky" environment map name "skyaxis" vector axis
- for rotating sky "skyrotate" speed of rotation in degrees/second "sounds"
- music cd track number "gravity" 800 is default gravity "message" text to
- print at user logon
-}
spWorldSpawn :: EntThink
spWorldSpawn =
  GenericEntThink "SP_worldspawn" $ \edictRef -> do
    edict <- readEdictT edictRef

    modifyEdictT edictRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                   & eSolid .~ Constants.solidBsp
                                   -- since the world doesn't use G_Spawn()
                                   & eInUse .~ True
                                   -- world model is always index 1
                                   & eEntityState.esModelIndex .~ 1)

    -- reserve some spots for dead player bodies for coop / deathmatch
    PlayerClient.initBodyQue
    -- set configstrings for items
    GameItems.setItemNames

    nextMap <- use $ gameBaseGlobals.gbSpawnTemp.stNextMap
    gameBaseGlobals.gbLevel.llNextMap .= nextMap

    -- make some data visible to the server
    gameImport <- use $ gameBaseGlobals.gbGameImport

    let configString = gameImport^.giConfigString
        imageIndex = gameImport^.giImageIndex
        soundIndex = gameImport^.giSoundIndex
        modelIndex = gameImport^.giModelIndex
        cvarSet = gameImport^.giCVarSet

    let msg = edict^.eMessage

    if isJust msg && B.length (fromJust msg) > 0
      then do
        configString Constants.csName (fromJust msg)
        gameBaseGlobals.gbLevel.llLevelName .= fromJust msg
      else do
        mapName <- use $ gameBaseGlobals.gbLevel.llMapName
        gameBaseGlobals.gbLevel.llLevelName .= mapName

    spawnTemp <- use $ gameBaseGlobals.gbSpawnTemp

    if B.length (spawnTemp^.stSky) > 0
      then configString Constants.csSky (spawnTemp^.stSky)
      else configString Constants.csSky "unit1_"

    configString Constants.csSkyRotate $ BC.pack (show (spawnTemp^.stSkyRotate)) -- IMPROVE ?
    configString Constants.csSkyAxis $ Lib.vtos (spawnTemp^.stSkyAxis)
    configString Constants.csCdTrack $ BC.pack (show (edict^.eSounds))

    maxClientsValue :: Int <- liftM (truncate . (^.cvValue)) maxClientsCVar
    configString Constants.csMaxClients $ BC.pack (show maxClientsValue) -- IMPROVE ?

    -- status bar program
    deathmatchValue <- liftM (^.cvValue) deathmatchCVar

    if deathmatchValue /= 0
      then configString Constants.csStatusBar dmStatusBar
      else configString Constants.csStatusBar singleStatusBar

    -- help icon for statusbar
    void $ imageIndex (Just "i_help")
    imageIndex (Just "i_health") >>= (gameBaseGlobals.gbLevel.llPicHealth .=)
    void $ imageIndex (Just "help")
    void $ imageIndex (Just "field_3")

    void $ case spawnTemp^.stGravity of
             Nothing -> cvarSet "sv_gravity" "800"
             Just gravity ->
               if gravity == ""
                 then cvarSet "sv_gravity" "800"
                 else cvarSet "sv_gravity" gravity

    soundIndex (Just "player/fry.wav") >>= (gameBaseGlobals.gbSndFry .=)

    -- standing in lava / slime
    GameItems.findItem "Blaster" >>= GameItems.precacheItem
    void $ soundIndex (Just "player/lava1.wav")
    void $ soundIndex (Just "player/lava2.wav")
    void $ soundIndex (Just "misc/pc_up.wav")
    void $ soundIndex (Just "misc/talk1.wav")
    void $ soundIndex (Just "misc/udeath.wav")
    -- gibs
    void $ soundIndex (Just "items/respawn1.wav")
    -- sexed sounds
    void $ soundIndex (Just "*death1.wav")
    void $ soundIndex (Just "*death2.wav")
    void $ soundIndex (Just "*death3.wav")
    void $ soundIndex (Just "*death4.wav")
    void $ soundIndex (Just "*fall1.wav")
    void $ soundIndex (Just "*fall2.wav")
    void $ soundIndex (Just "*gurp1.wav")
    -- drowning damage
    void $ soundIndex (Just "*gurp2.wav")
    void $ soundIndex (Just "*jump1.wav")
    -- player jump
    void $ soundIndex (Just "*pain25_1.wav")
    void $ soundIndex (Just "*pain25_2.wav")
    void $ soundIndex (Just "*pain50_1.wav")
    void $ soundIndex (Just "*pain50_2.wav")
    void $ soundIndex (Just "*pain75_1.wav")
    void $ soundIndex (Just "*pain75_2.wav")
    void $ soundIndex (Just "*pain100_1.wav")
    void $ soundIndex (Just "*pain100_2.wav")
    -- sexed models
    -- THIS ORDER MUST MATCH THE DEFINES IN g_local.h
    -- you can add more, max 15
    void $ modelIndex (Just "#w_blaster.md2")
    void $ modelIndex (Just "#w_shotgun.md2")
    void $ modelIndex (Just "#w_sshotgun.md2")
    void $ modelIndex (Just "#w_machinegun.md2")
    void $ modelIndex (Just "#w_chaingun.md2")
    void $ modelIndex (Just "#a_grenades.md2")
    void $ modelIndex (Just "#w_glauncher.md2")
    void $ modelIndex (Just "#w_rlauncher.md2")
    void $ modelIndex (Just "#w_hyperblaster.md2")
    void $ modelIndex (Just "#w_railgun.md2")
    void $ modelIndex (Just "#w_bfg.md2")
    -- --------------
    void $ soundIndex (Just "player/gasp1.wav")
    -- gasping for air
    void $ soundIndex (Just "player/gasp2.wav")
    -- head breaking surface, not gasping
    void $ soundIndex (Just "player/watr_in.wav")
    -- feet hitting water
    void $ soundIndex (Just "player/watr_out.wav")
    -- feet leaving water
    void $ soundIndex (Just "player/watr_un.wav")
    -- head going underwater
    void $ soundIndex (Just "player/u_breath1.wav")
    void $ soundIndex (Just "player/u_breath2.wav")
    void $ soundIndex (Just "items/pkup.wav")
    -- bonus item pickup
    void $ soundIndex (Just "world/land.wav")
    -- landing thud
    void $ soundIndex (Just "misc/h2ohit1.wav")
    -- landing splash
    void $ soundIndex (Just "items/damage.wav")
    void $ soundIndex (Just "items/protect.wav")
    void $ soundIndex (Just "items/protect4.wav")
    void $ soundIndex (Just "weapons/noammo.wav")
    void $ soundIndex (Just "infantry/inflies1.wav")
    modelIndex (Just "models/objects/gibs/sm_meat/tris.md2") >>= (gameBaseGlobals.gbSmMeatIndex .=)
    void $ modelIndex (Just "models/objects/gibs/arm/tris.md2")
    void $ modelIndex (Just "models/objects/gibs/bone/tris.md2")
    void $ modelIndex (Just "models/objects/gibs/bone2/tris.md2")
    void $ modelIndex (Just "models/objects/gibs/chest/tris.md2")
    void $ modelIndex (Just "models/objects/gibs/skull/tris.md2")
    void $ modelIndex (Just "models/objects/gibs/head2/tris.md2")

    --
    -- Setup light animation tables. 'a' is total darkness, 'z' is
    -- doublebright.
    --
    -- 0 normal
    configString (Constants.csLights + 0) "m"
    -- 1 FLICKER (first variety)
    configString (Constants.csLights + 1) "mmnmmommommnonmmonqnmmo"
    -- 2 SLOW STRONG PULSE
    configString (Constants.csLights + 2) "abcdefghijklmnopqrstuvwxyzyxwvutsrqponmlkjihgfedcba"
    -- 3 CANDLE (first variety)
    configString (Constants.csLights + 3) "mmmmmaaaaammmmmaaaaaabcdefgabcdefg"
    -- 4 FAST STROBE
    configString (Constants.csLights + 4) "mamamamamama"
    -- 5 GENTLE PULSE 1
    configString (Constants.csLights + 5) "jklmnopqrstuvwxyzyxwvutsrqponmlkj"
    -- 6 FLICKER (second variety)
    configString (Constants.csLights + 6) "nmonqnmomnmomomno"
    -- 7 CANDLE (second variety)
    configString (Constants.csLights + 7) "mmmaaaabcdefgmmmmaaaammmaamm"
    -- 8 CANDLE (third variety)
    configString (Constants.csLights + 8) "mmmaaammmaaammmabcdefaaaammmmabcdefmmmaaaa"
    -- 9 SLOW STROBE (fourth variety)
    configString (Constants.csLights + 9) "aaaaaaaazzzzzzzz"
    -- 10 FLUORESCENT FLICKER
    configString (Constants.csLights + 10) "mmamammmmammamamaaamammma"
    -- 11 SLOW PULSE NOT FADE TO BLACK
    configString (Constants.csLights + 11) "abcdefghijklmnopqrrqponmlkjihgfedcba"
    -- styles 32-62 are assigned by the light program for switchable
    -- lights
    -- 63 testing
    configString (Constants.csLights + 63) "a"

    return True

spFuncWall :: EntThink
spFuncWall =
  GenericEntThink "SP_func_wall" $ \edictRef -> do
    GameMisc.spFuncWall edictRef
    return True

spFuncObject :: EntThink
spFuncObject =
  GenericEntThink "SP_func_object" $ \edictRef -> do
    GameMisc.spFuncObject edictRef
    return True

spFuncTimer :: EntThink
spFuncTimer =
  GenericEntThink "SP_func_timer" $ \edictRef -> do
    GameFunc.spFuncTimer edictRef
    return True

spFuncExplosive :: EntThink
spFuncExplosive =
  GenericEntThink "SP_func_explosive" $ \edictRef -> do
    GameMisc.spFuncExplosive edictRef
    return True

spTriggerAlways :: EntThink
spTriggerAlways =
  GenericEntThink "SP_trigger_always" $ \edictRef -> do
    GameTrigger.spTriggerAlways edictRef
    return True

spTriggerOnce :: EntThink
spTriggerOnce =
  GenericEntThink "SP_trigger_once" $ \edictRef -> do
    GameTrigger.spTriggerOnce edictRef
    return True

spTriggerMultiple :: EntThink
spTriggerMultiple =
  GenericEntThink "SP_trigger_multiple" $ \edictRef -> do
    GameTrigger.spTriggerMultiple edictRef
    return True

spTriggerRelay :: EntThink
spTriggerRelay =
  GenericEntThink "SP_trigger_relay" $ \edictRef -> do
    GameTrigger.spTriggerRelay edictRef
    return True

spTriggerPush :: EntThink
spTriggerPush =
  GenericEntThink "SP_trigger_push" $ \edictRef -> do
    GameTrigger.spTriggerPush edictRef
    return True

spTriggerHurt :: EntThink
spTriggerHurt =
  GenericEntThink "SP_trigger_hurt" $ \edictRef -> do
    GameTrigger.spTriggerHurt edictRef
    return True

spTriggerKey :: EntThink
spTriggerKey =
  GenericEntThink "SP_trigger_key" $ \edictRef -> do
    GameTrigger.spTriggerKey edictRef
    return True

spTriggerCounter :: EntThink
spTriggerCounter =
  GenericEntThink "SP_trigger_counter" $ \edictRef -> do
    GameTrigger.spTriggerCounter edictRef
    return True

spTriggerGravity :: EntThink
spTriggerGravity =
  GenericEntThink "SP_trigger_gravity" $ \edictRef -> do
    GameTrigger.spTriggerGravity edictRef
    return True

spTriggerMonsterJump :: EntThink
spTriggerMonsterJump =
  GenericEntThink "SP_trigger_monsterjump" $ \edictRef -> do
    GameTrigger.spTriggerMonsterJump edictRef
    return True

spTargetTempEntity :: EntThink
spTargetTempEntity =
  GenericEntThink "SP_target_temp_entity" $ \edictRef -> do
    GameTarget.spTargetTempEntity edictRef
    return True

spTargetSpeaker :: EntThink
spTargetSpeaker =
  GenericEntThink "SP_target_speaker" $ \edictRef -> do
    GameTarget.spTargetSpeaker edictRef
    return True

spTargetExplosion :: EntThink
spTargetExplosion =
  GenericEntThink "SP_target_explosion" $ \edictRef -> do
    GameTarget.spTargetExplosion edictRef
    return True

spTargetChangeLevel :: EntThink
spTargetChangeLevel =
  GenericEntThink "SP_target_changelevel" $ \edictRef -> do
    GameTarget.spTargetChangeLevel edictRef
    return True

spTargetSecret :: EntThink
spTargetSecret =
  GenericEntThink "SP_target_secret" $ \edictRef -> do
    GameTarget.spTargetSecret edictRef
    return True

spTargetGoal :: EntThink
spTargetGoal =
  GenericEntThink "SP_target_goal" $ \edictRef -> do
    GameTarget.spTargetGoal edictRef
    return True


spTargetSplash :: EntThink
spTargetSplash =
  GenericEntThink "SP_target_splash" $ \edictRef -> do
    GameTarget.spTargetSplash edictRef
    return True

spTargetSpawner :: EntThink
spTargetSpawner =
  GenericEntThink "SP_target_spawner" $ \edictRef -> do
    GameTarget.spTargetSpawner edictRef
    return True

spTargetBlaster :: EntThink
spTargetBlaster =
  GenericEntThink "SP_target_blaster" $ \edictRef -> do
    GameTarget.spTargetBlaster edictRef
    return True

spTargetCrossLevelTrigger :: EntThink
spTargetCrossLevelTrigger =
  GenericEntThink "SP_target_crosslevel_trigger" $ \edictRef -> do
    GameTarget.spTargetCrossLevelTrigger edictRef
    return True

spTargetCrossLevelTarget :: EntThink
spTargetCrossLevelTarget =
  GenericEntThink "SP_target_crosslevel_target" $ \edictRef -> do
    GameTarget.spTargetCrossLevelTarget edictRef
    return True

spTargetLaser :: EntThink
spTargetLaser =
  GenericEntThink "SP_target_laser" $ \edictRef -> do
    GameTarget.spTargetLaser edictRef
    return True

spTargetHelp :: EntThink
spTargetHelp =
  GenericEntThink "SP_target_help" $ \edictRef -> do
    GameTarget.spTargetHelp edictRef
    return True

spTargetActor :: EntThink
spTargetActor =
  GenericEntThink "SP_target_actor" $ \edictRef -> do
    MActor.spTargetActor edictRef
    return True

spTargetLightRamp :: EntThink
spTargetLightRamp =
  GenericEntThink "SP_target_lightramp" $ \edictRef -> do
    GameTarget.spTargetLightRamp edictRef
    return True

spTargetEarthquake :: EntThink
spTargetEarthquake =
  GenericEntThink "SP_target_earthquake" $ \edictReference -> do
    GameTarget.spTargetEarthquake edictReference
    return True

spTargetCharacter :: EntThink
spTargetCharacter =
  GenericEntThink "SP_target_character" $ \edictReference -> do
    GameMisc.spTargetCharacter edictReference
    return True

spTargetString :: EntThink
spTargetString =
  GenericEntThink "SP_target_string" $ \edictReference -> do
    GameMisc.spTargetString edictReference
    return True

spViewThing :: EntThink
spViewThing =
  GenericEntThink "SP_viewthing" $ \edictReference -> do
    GameMisc.spViewThing edictReference
    return True

spLight :: EntThink
spLight =
  GenericEntThink "SP_light" $ \edictReference -> do
    GameMisc.spLight edictReference
    return True

spLightMine1 :: EntThink
spLightMine1 =
  GenericEntThink "SP_light_mine1" $ \edictReference -> do
    GameMisc.spLightMine1 edictReference
    return True

spLightMine2 :: EntThink
spLightMine2 =
  GenericEntThink "SP_light_mine2" $ \edictReference -> do
    GameMisc.spLightMine2 edictReference
    return True

spInfoNull :: EntThink
spInfoNull =
  GenericEntThink "SP_info_null" $ \edictReference -> do
    GameMisc.spInfoNull edictReference
    return True

spInfoNotNull :: EntThink
spInfoNotNull =
  GenericEntThink "SP_info_notnull" $ \edictReference -> do
    GameMisc.spInfoNotNull edictReference
    return True

spPathCorner :: EntThink
spPathCorner =
  GenericEntThink "SP_path_corner" $ \edictReference -> do
    GameMisc.spPathCorner edictReference
    return True

spPointCombat :: EntThink
spPointCombat =
  GenericEntThink "SP_point_combat" $ \edictReference -> do
    GameMisc.spPointCombat edictReference
    return True

spMiscExploBox :: EntThink
spMiscExploBox =
  GenericEntThink "SP_misc_explobox" $ \edictReference -> do
    GameMisc.spMiscExploBox edictReference
    return True

spMiscBanner :: EntThink
spMiscBanner =
  GenericEntThink "SP_misc_banner" $ \edictReference -> do
    GameMisc.spMiscBanner edictReference
    return True

spMiscSatelliteDish :: EntThink
spMiscSatelliteDish =
  GenericEntThink "SP_misc_satellite_dish" $ \edictReference -> do
    GameMisc.spMiscSatelliteDish edictReference
    return True

spMiscActor :: EntThink
spMiscActor =
  GenericEntThink "SP_misc_actor" $ \edictReference -> do
    MActor.spMiscActor edictReference
    return False

spMiscGibArm :: EntThink
spMiscGibArm =
  GenericEntThink "SP_misc_gib_arm" $ \edictReference -> do
    GameMisc.spMiscGibArm edictReference
    return True

spMiscGibLeg :: EntThink
spMiscGibLeg =
  GenericEntThink "SP_misc_gib_leg" $ \edictReference -> do
    GameMisc.spMiscGibLeg edictReference
    return True

spMiscGibHead :: EntThink
spMiscGibHead =
  GenericEntThink "SP_misc_gib_head" $ \edictReference -> do
    GameMisc.spMiscGibHead edictReference
    return True

spMiscInsane :: EntThink
spMiscInsane =
  GenericEntThink "SP_misc_insane" $ \edictReference -> do
    MInsane.spMiscInsane edictReference
    return True

spMiscDeadSoldier :: EntThink
spMiscDeadSoldier =
  GenericEntThink "SP_misc_deadsoldier" $ \edictReference -> do
    GameMisc.spMiscDeadSoldier edictReference
    return True

spMiscViper :: EntThink
spMiscViper =
  GenericEntThink "SP_misc_viper" $ \edictReference -> do
    GameMisc.spMiscViper edictReference
    return True

spMiscViperBomb :: EntThink
spMiscViperBomb =
  GenericEntThink "SP_misc_viper_bomb" $ \edictReference -> do
    GameMisc.spMiscViperBomb edictReference
    return True

spMiscBigViper :: EntThink
spMiscBigViper =
  GenericEntThink "SP_misc_bigviper" $ \edictReference -> do
    GameMisc.spMiscBigViper edictReference
    return True

spMiscStroggShip :: EntThink
spMiscStroggShip =
  GenericEntThink "SP_misc_strogg_ship" $ \edictReference -> do
    GameMisc.spMiscStroggShip edictReference
    return True

spMiscTeleporter :: EntThink
spMiscTeleporter =
  GenericEntThink "SP_misc_teleporter" $ \edictReference -> do
    GameMisc.spMiscTeleporter edictReference
    return True

spMiscBlackHole :: EntThink
spMiscBlackHole =
  GenericEntThink "SP_misc_blackhole" $ \edictReference -> do
    GameMisc.spMiscBlackHole edictReference
    return True

spMiscEasterTank :: EntThink
spMiscEasterTank =
  GenericEntThink "SP_misc_eastertank" $ \edictReference -> do
    GameMisc.spMiscEasterTank edictReference
    return True

spMiscEasterChick :: EntThink
spMiscEasterChick =
  GenericEntThink "SP_misc_easterchick" $ \edictReference -> do
    GameMisc.spMiscEasterChick edictReference
    return True

spMiscEasterChick2 :: EntThink
spMiscEasterChick2 =
  GenericEntThink "SP_misc_easterchick2" $ \edictReference -> do
    GameMisc.spMiscEasterChick2 edictReference
    return True

spMonsterBerserk :: EntThink
spMonsterBerserk =
  GenericEntThink "SP_monster_berserk" $ \edictReference -> do
    MBerserk.spMonsterBerserk edictReference
    return True

spMonsterGladiator :: EntThink
spMonsterGladiator =
  GenericEntThink "SP_monster_gladiator" $ \edictReference -> do
    MGladiator.spMonsterGladiator edictReference
    return True

spMonsterGunner :: EntThink
spMonsterGunner =
  GenericEntThink "SP_monster_gunner" $ \edictReference -> do
    MGunner.spMonsterGunner edictReference
    return True

spMonsterInfantry :: EntThink
spMonsterInfantry =
  GenericEntThink "SP_monster_infantry" $ \edictReference -> do
    MInfantry.spMonsterInfantry edictReference
    return True

spMonsterMedic :: EntThink
spMonsterMedic =
  GenericEntThink "SP_monster_medic" $ \edictReference -> do
    MMedic.spMonsterMedic edictReference
    return True

spMonsterFlipper :: EntThink
spMonsterFlipper =
  GenericEntThink "SP_monster_flipper" $ \edictReference -> do
    MFlipper.spMonsterFlipper edictReference
    return True

spMonsterChick :: EntThink
spMonsterChick =
  GenericEntThink "SP_monster_chick" $ \edictReference -> do
    MChick.spMonsterChick edictReference
    return True

spMonsterFlyer :: EntThink
spMonsterFlyer =
  GenericEntThink "SP_monster_flyer" $ \edictReference -> do
    MFlyer.spMonsterFlyer edictReference
    return True

spMonsterBrain :: EntThink
spMonsterBrain =
  GenericEntThink "SP_monster_brain" $ \edictReference -> do
    MBrain.spMonsterBrain edictReference
    return True

spMonsterFloater :: EntThink
spMonsterFloater =
  GenericEntThink "SP_monster_floater" $ \edictReference -> do
    MFloat.spMonsterFloater edictReference
    return True

spMonsterHover :: EntThink
spMonsterHover =
  GenericEntThink "SP_monster_hover" $ \edictReference -> do
    MHover.spMonsterHover edictReference
    return True

spMonsterBoss2 :: EntThink
spMonsterBoss2 =
  GenericEntThink "SP_monster_boss2" $ \edictReference -> do
    MBoss2.spMonsterBoss2 edictReference
    return True

spMonsterBoss3Stand :: EntThink
spMonsterBoss3Stand =
  GenericEntThink "SP_monster_boss3_stand" $ \edictReference -> do
    MBoss3.spMonsterBoss3Stand edictReference
    return True

spMonsterJorg :: EntThink
spMonsterJorg =
  GenericEntThink "SP_monster_jorg" $ \edictReference -> do
    MBoss31.spMonsterJorg edictReference
    return True

spMonsterCommanderBody :: EntThink
spMonsterCommanderBody =
  GenericEntThink "SP_monster_commander_body" $ \edictReference -> do
    GameMisc.spMonsterCommanderBody edictReference
    return True

spTurretBreach :: EntThink
spTurretBreach =
  GenericEntThink "SP_turret_breach" $ \edictReference -> do
    GameTurret.spTurretBreach edictReference
    return True

spTurretBase :: EntThink
spTurretBase =
  GenericEntThink "SP_turret_base" $ \edictReference -> do
    GameTurret.spTurretBase edictReference
    return True

spTurretDriver :: EntThink
spTurretDriver =
  GenericEntThink "SP_turret_driver" $ \edictReference -> do
    GameTurret.spTurretDriver edictReference
    return True

-- learned something new here... original source has tabs, so had to change them for \t
-- http://vim.wikia.com/wiki/See_the_tabs_in_your_file
singleStatusBar :: B.ByteString
singleStatusBar = BC.pack $
               "yb\t-24 " -- health
            ++ "xv\t0 " ++ "hnum " ++ "xv\t50 " ++ "pic 0 " -- ammo
            ++ "if 2 " ++ "\txv\t100 " ++ "\tanum " ++ "\txv\t150 " ++ "\tpic 2 "
            ++ "endif " -- armor
            ++ "if 4 " ++ "\txv\t200 " ++ "\trnum " ++ "\txv\t250 " ++ "\tpic 4 "
            ++ "endif " -- selected item
            ++ "if 6 " ++ "\txv\t296 " ++ "\tpic 6 " ++ "endif " ++ "yb\t-50 " -- picked
            -- up
            -- item
            ++ "if 7 " ++ "\txv\t0 " ++ "\tpic 7 " ++ "\txv\t26 " ++ "\tyb\t-42 "
            ++ "\tstat_string 8 " ++ "\tyb\t-50 " ++ "endif "
            -- timer
            ++ "if 9 " ++ "\txv\t262 " ++ "\tnum\t2\t10 " ++ "\txv\t296 " ++ "\tpic\t9 "
            ++ "endif "
            -- help / weapon icon
            ++ "if 11 " ++ "\txv\t148 " ++ "\tpic\t11 " ++ "endif "

dmStatusBar :: B.ByteString
dmStatusBar = BC.pack $
               "yb\t-24 " -- health
            ++ "xv\t0 " ++ "hnum " ++ "xv\t50 " ++ "pic 0 " --  ammo
            ++ "if 2 " ++ "\txv\t100 " ++ "\tanum " ++ "\txv\t150 " ++ "\tpic 2 "
            ++ "endif " -- armor
            ++ "if 4 " ++ "\txv\t200 " ++ "\trnum " ++ "\txv\t250 " ++ "\tpic 4 "
            ++ "endif " -- selected item
            ++ "if 6 " ++ "\txv\t296 " ++ "\tpic 6 " ++ "endif " ++ "yb\t-50 " -- picked
            -- up
            -- item
            ++ "if 7 " ++ "\txv\t0 " ++ "\tpic 7 " ++ "\txv\t26 " ++ "\tyb\t-42 "
            ++ "\tstat_string 8 " ++ "\tyb\t-50 " ++ "endif "
            -- timer
            ++ "if 9 " ++ "\txv\t246 " ++ "\tnum\t2\t10 " ++ "\txv\t296 " ++ "\tpic\t9 "
            ++ "endif "
            -- help / weapon icon
            ++ "if 11 " ++ "\txv\t148 " ++ "\tpic\t11 " ++ "endif " -- frags
            ++ "xr\t-50 " ++ "yt 2 " ++ "num 3 14 " -- spectator
            ++ "if 17 " ++ "xv 0 " ++ "yb -58 " ++ "string2 \"SPECTATOR MODE\" "
            ++ "endif " -- chase camera
            ++ "if 16 " ++ "xv 0 " ++ "yb -68 " ++ "string \"Chasing\" " ++ "xv 64 "
            ++ "stat_string 16 " ++ "endif "
