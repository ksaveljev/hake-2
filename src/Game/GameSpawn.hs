{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameSpawn where

import Control.Lens (use, (^.), (.=), (%=), preuse, ix)
import Control.Monad (liftM, when, void, unless)
import Data.Bits ((.&.), complement, (.|.))
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust)
import Linear (V3(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import Game.Adapters
import Game.SpawnT
import qualified Constants
import qualified Game.GameFunc as GameFunc
import qualified Game.GameItems as GameItems
import qualified Game.GameMisc as GameMisc
import qualified Game.GameTarget as GameTarget
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
import qualified Game.Monsters.MMedic as MMedic
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
    let newEdicts = map (\i -> (i, newEdictT i)) [0..maxEntities-1]
    gameBaseGlobals.gbGEdicts %= (V.// newEdicts)

    gameBaseGlobals.gbLevel.llMapName .= mapName
    gameBaseGlobals.gbGame.glSpawnPoint .= spawnPoint

    -- set client fields on player ents
    maxClients <- use $ gameBaseGlobals.gbGame.glMaxClients
    edicts <- use $ gameBaseGlobals.gbGEdicts
    let updatedEdicts = V.imap (\idx edict -> if idx >= 1 && idx <= maxClients then edict { _eClient = Just (GClientReference (idx - 1)) } else edict) edicts
    gameBaseGlobals.gbGEdicts .= updatedEdicts

    inhibited <- parseEntities True 0 0

    Com.dprintf $ "player skill level:" `B.append` BC.pack (show skillValue) `B.append` "\n" -- IMPROVE
    Com.dprintf $ BC.pack (show inhibited) `B.append` " entities inhibited.\n"

    findTeams
    PlayerTrail.init

  where parseEntities :: Bool -> Int -> Int -> Quake Int
        parseEntities initial idx inhibited
          | idx == B.length entities = return inhibited
          | otherwise = do
              (comToken, newIdx) <- Com.parse entities (B.length entities) idx

              case comToken of
                Nothing -> return inhibited
                Just token -> do

                  when (BC.head token /= '{') $ do
                    err <- use $ gameBaseGlobals.gbGameImport.giError
                    err $ "ED_LoadFromFile: found " `B.append` token `B.append` " when expecting {"

                  ent@(EdictReference edictIdx) <- if initial
                                                     then return (EdictReference 0)
                                                     else GameUtil.spawn

                  updatedIdx <- parseEdict ent entities newIdx

                  Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx
                  Com.dprintf $ "spawning ent[" `B.append` BC.pack (show $ edict^.eIndex) `B.append` -- IMPROVE
                                "], classname=" `B.append` (edict^.eClassName) `B.append`
                                ", flags=" `B.append` BC.pack (show $ edict^.eSpawnFlags) -- IMPROVE: show flags in hex

                  -- yet another map hack
                  when (BC.map toLower mapName == "command" &&
                        BC.map toLower (edict^.eClassName) == "trigger_once" &&
                        isJust (edict^.eEdictInfo.eiModel) &&
                        BC.map toLower (fromJust (edict^.eEdictInfo.eiModel)) == "*27") $
                    gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags %= (.&. (complement Constants.spawnFlagNotHard))

                  -- remove things (except the world) from different skill levels or deathmatch
                  removed <- if edictIdx == 0
                               then return False
                               else do
                                 deathmatchValue <- liftM (^.cvValue) deathmatchCVar

                                 freed <- if deathmatchValue /= 0
                                            then
                                              if ((edict^.eSpawnFlags) .&. Constants.spawnFlagNotDeathmatch) /= 0
                                                then do
                                                  Com.dprintf "->inhibited.\n"
                                                  GameUtil.freeEdict ent
                                                  return True
                                                else return False
                                            else do
                                              skillValue <- liftM (^.cvValue) skillCVar
                                              if (skillValue == 0 && (edict^.eSpawnFlags .&. Constants.spawnFlagNotEasy) /= 0) ||
                                                 (skillValue == 1 && (edict^.eSpawnFlags .&. Constants.spawnFlagNotMedium) /= 0) ||
                                                 ((skillValue == 2 || skillValue == 3) && (edict^.eSpawnFlags .&. Constants.spawnFlagNotHard) /= 0)
                                                 then do
                                                   Com.dprintf "->inhibited.\n"
                                                   GameUtil.freeEdict ent
                                                   return True
                                                 else return False

                                 if freed
                                   then do
                                     let flags = Constants.spawnFlagNotEasy .|. Constants.spawnFlagNotMedium .|. Constants.spawnFlagNotHard .|. Constants.spawnFlagNotCoop .|. Constants.spawnFlagNotDeathmatch
                                     gameBaseGlobals.gbGEdicts.ix edictIdx.eSpawnFlags %= (.&. (complement flags))
                                     return True
                                   else do
                                     return False


                  if removed
                    then parseEntities False updatedIdx (inhibited + 1)
                    else do
                      callSpawn ent
                      Com.dprintf "\n"

                      parseEntities False updatedIdx inhibited

{-
- ED_ParseEdict
-
- Parses an edict out of the given string, returning the new position ed
- should be a properly initialized empty edict.
-}
parseEdict :: EdictReference -> B.ByteString -> Int -> Quake Int
parseEdict er entities idx = do
    gameBaseGlobals.gbSpawnTemp .= newSpawnTempT

    (newIdx, initial) <- parse False idx

    unless initial $ GameUtil.clearEdict er

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
                then return (newIdx, initial)
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
                        then parse True finalIdx
                        else do
                          parseField (BC.map toLower keyName) anotherToken er
                          parse True finalIdx

parseField :: B.ByteString -> B.ByteString -> EdictReference -> Quake ()
parseField key value (EdictReference idx) = do
    when (key == "nextmap") $
      Com.println $ "nextmap: " `B.append` value

    st <- use $ gameBaseGlobals.gbSpawnTemp
    let (updatedSt, didUpdateSpawnTemp) = setSpawnTempField st key value

    if didUpdateSpawnTemp
      then gameBaseGlobals.gbSpawnTemp .= updatedSt
      else do
        Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix idx
        let (updatedEdict, didUpdateEdict) = setEdictField edict key value
        if didUpdateEdict
          then gameBaseGlobals.gbGEdicts.ix idx .= updatedEdict
          else do
            dprintf <- use $ gameBaseGlobals.gbGameImport.giDprintf
            dprintf $ "??? The key [" `B.append` key `B.append` "] is not a field\n"

setSpawnTempField :: SpawnTempT -> B.ByteString -> B.ByteString -> (SpawnTempT, Bool)
setSpawnTempField st key value =
    case key of
      "lip"       -> (st { _stLip       = Lib.atoi  value }, True)
      "distance"  -> (st { _stDistance  = Lib.atoi  value }, True)
      "height"    -> (st { _stHeight    = Lib.atoi  value }, True)
      "noise"     -> (st { _stNoise     = newString value }, True)
      "pausetime" -> (st { _stPauseTime = Lib.atof  value }, True)
      "item"      -> (st { _stItem      = newString value }, True)
      "gravity"   -> (st { _stGravity   = newString value }, True)
      "sky"       -> (st { _stSky       = newString value }, True)
      "skyrotate" -> (st { _stSkyRotate = Lib.atof  value }, True)
      "skyaxis"   -> (st { _stSkyAxis   = Lib.atov  value }, True)
      "minyaw"    -> (st { _stMinYaw    = Lib.atof  value }, True)
      "maxyaw"    -> (st { _stMaxYaw    = Lib.atof  value }, True)
      "minpitch"  -> (st { _stMinPitch  = Lib.atof  value }, True)
      "maxpitch"  -> (st { _stMaxPitch  = Lib.atof  value }, True)
      "nextmap"   -> (st { _stNextMap   = newString value }, True)
      _           -> (st, False)

setEdictField :: EdictT -> B.ByteString -> B.ByteString -> (EdictT, Bool)
setEdictField e key value =
    case key of
      "classname"    -> (e { _eClassName = newString value }, True)
      "model"        -> (e { _eEdictInfo = (e^.eEdictInfo) { _eiModel = Just (newString value) } }, True)
      "spawnflags"   -> (e { _eSpawnFlags = Lib.atoi value }, True)
      "speed"        -> (e { _eEdictPhysics = (e^.eEdictPhysics) { _eSpeed = Lib.atof value } }, True )
      "accel"        -> (e { _eEdictPhysics = (e^.eEdictPhysics) { _eAccel = Lib.atof value } }, True )
      "decel"        -> (e { _eEdictPhysics = (e^.eEdictPhysics) { _eDecel = Lib.atof value } }, True )
      "target"       -> (e { _eEdictInfo = (e^.eEdictInfo) { _eiTarget = Just (newString value) } }, True)
      "targetname"   -> (e { _eEdictInfo = (e^.eEdictInfo) { _eiTargetName = Just (newString value) } }, True)
      "pathtarget"   -> (e { _eEdictInfo = (e^.eEdictInfo) { _eiPathTarget = Just (newString value) } }, True)
      "deathtarget"  -> (e { _eEdictInfo = (e^.eEdictInfo) { _eiDeathTarget = Just (newString value) } }, True)
      "killtarget"   -> (e { _eEdictInfo = (e^.eEdictInfo) { _eiKillTarget = Just (newString value) } }, True)
      "combattarget" -> (e { _eEdictInfo = (e^.eEdictInfo) { _eiCombatTarget = Just (newString value) } }, True)
      "message"      -> (e { _eEdictInfo = (e^.eEdictInfo) { _eiMessage = Just (newString value) } }, True)
      "team"         -> (e { _eEdictInfo = (e^.eEdictInfo) { _eiTeam = Just (newString value) } }, True) -- TODO: we need to call Com.dprintf here
      "wait"         -> (e { _eWait = Lib.atof value }, True)
      "delay"        -> (e { _eDelay = Lib.atof value }, True)
      "random"       -> (e { _eRandom = Lib.atof value }, True)
      "move_origin"  -> (e { _eMoveOrigin = Lib.atov value }, True)
      "move_angles"  -> (e { _eMoveAngles = Lib.atov value }, True)
      "style"        -> (e { _eStyle = Lib.atoi value }, True)
      "count"        -> (e { _eCount = Lib.atoi value }, True)
      "health"       -> (e { _eEdictStatus = (e^.eEdictStatus) { _eHealth = Lib.atoi value } }, True)
      "sounds"       -> (e { _eSounds = Lib.atoi value }, True)
      "light"        -> (e, True)
      "dmg"          -> (e { _eEdictStatus = (e^.eEdictStatus) { _eDmg = Lib.atoi value } }, True)
      "mass"         -> (e { _eEdictPhysics = (e^.eEdictPhysics) { _eMass = Lib.atoi value } }, True)
      "volume"       -> (e { _eVolume = Lib.atof value }, True)
      "attenuation"  -> (e { _eAttenuation = Lib.atof value }, True)
      "map"          -> (e { _eEdictInfo = (e^.eEdictInfo) { _eiMap = Just (newString value) } }, True)
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
callSpawn er@(EdictReference edictIdx) = do
    numItems <- use $ gameBaseGlobals.gbGame.glNumItems
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    -- IMPROVE: does it apply to our code?
    -- if (null == ent.classname) {
    --     GameBase.gi.dprintf("ED_CallSpawn: null classname\n");
    --     return;
    -- }

    -- check item spawn functions
    let edictClassName = BC.map toLower (edict^.eClassName)
    itemSpawnIndex <- checkItemSpawn edictClassName 1 numItems

    case itemSpawnIndex of
      Just gItemReference -> GameItems.spawnItem er gItemReference
      Nothing -> do
        -- check normal spawn functions
        let spawnIdx = V.findIndex (\s -> edictClassName == BC.map toLower (s^.spName)) spawns

        case spawnIdx of
          Just idx ->
            void $ think ((spawns V.! idx)^.spSpawn) er
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

findTeams :: Quake ()
findTeams = io (putStrLn "GameSpawn.findTeams") >> undefined -- TODO

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
  GenericEntThink "SP_worldspawn" $ \(EdictReference edictIdx) -> do
    Just edict <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx

    gameBaseGlobals.gbGEdicts.ix edictIdx .= edict { _eMoveType    = Constants.moveTypePush
                                                   , _eSolid       = Constants.solidBsp
                                                   -- since the world doesn't use G_Spawn()
                                                   , _eInUse       = True
                                                   -- world model is always index 1
                                                   , _eEntityState = (edict^.eEntityState) { _esModelIndex = 1 }
                                                   }

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

    let msg = edict^.eEdictInfo.eiMessage
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
    void $ imageIndex "i_help"
    imageIndex "i_health" >>= (gameBaseGlobals.gbLevel.llPicHealth .=)
    void $ imageIndex "help"
    void $ imageIndex "field_3"

    if (spawnTemp^.stGravity) == ""
      then void $ cvarSet "sv_gravity" "800"
      else void $ cvarSet "sv_gravity" (spawnTemp^.stGravity)

    soundIndex "player/fry.wav" >>= (gameBaseGlobals.gbSndFry .=)

    -- standing in lava / slime
    GameItems.findItem "Blaster" >>= GameItems.precacheItem
    void $ soundIndex "player/lava1.wav"
    void $ soundIndex "player/lava2.wav"
    void $ soundIndex "misc/pc_up.wav"
    void $ soundIndex "misc/talk1.wav"
    void $ soundIndex "misc/udeath.wav"
    -- gibs
    void $ soundIndex "items/respawn1.wav"
    -- sexed sounds
    void $ soundIndex "*death1.wav"
    void $ soundIndex "*death2.wav"
    void $ soundIndex "*death3.wav"
    void $ soundIndex "*death4.wav"
    void $ soundIndex "*fall1.wav"
    void $ soundIndex "*fall2.wav"
    void $ soundIndex "*gurp1.wav"
    -- drowning damage
    void $ soundIndex "*gurp2.wav"
    void $ soundIndex "*jump1.wav"
    -- player jump
    void $ soundIndex "*pain25_1.wav"
    void $ soundIndex "*pain25_2.wav"
    void $ soundIndex "*pain50_1.wav"
    void $ soundIndex "*pain50_2.wav"
    void $ soundIndex "*pain75_1.wav"
    void $ soundIndex "*pain75_2.wav"
    void $ soundIndex "*pain100_1.wav"
    void $ soundIndex "*pain100_2.wav"
    -- sexed models
    -- THIS ORDER MUST MATCH THE DEFINES IN g_local.h
    -- you can add more, max 15
    void $ modelIndex "#w_blaster.md2"
    void $ modelIndex "#w_shotgun.md2"
    void $ modelIndex "#w_sshotgun.md2"
    void $ modelIndex "#w_machinegun.md2"
    void $ modelIndex "#w_chaingun.md2"
    void $ modelIndex "#a_grenades.md2"
    void $ modelIndex "#w_glauncher.md2"
    void $ modelIndex "#w_rlauncher.md2"
    void $ modelIndex "#w_hyperblaster.md2"
    void $ modelIndex "#w_railgun.md2"
    void $ modelIndex "#w_bfg.md2"
    -- --------------
    void $ soundIndex "player/gasp1.wav"
    -- gasping for air
    void $ soundIndex "player/gasp2.wav"
    -- head breaking surface, not gasping
    void $ soundIndex "player/watr_in.wav"
    -- feet hitting water
    void $ soundIndex "player/watr_out.wav"
    -- feet leaving water
    void $ soundIndex "player/watr_un.wav"
    -- head going underwater
    void $ soundIndex "player/u_breath1.wav"
    void $ soundIndex "player/u_breath2.wav"
    void $ soundIndex "items/pkup.wav"
    -- bonus item pickup
    void $ soundIndex "world/land.wav"
    -- landing thud
    void $ soundIndex "misc/h2ohit1.wav"
    -- landing splash
    void $ soundIndex "items/damage.wav"
    void $ soundIndex "items/protect.wav"
    void $ soundIndex "items/protect4.wav"
    void $ soundIndex "weapons/noammo.wav"
    void $ soundIndex "infantry/inflies1.wav"
    modelIndex "models/objects/gibs/sm_meat/tris.md2" >>= (gameBaseGlobals.gbSmMeatIndex .=)
    void $ modelIndex "models/objects/gibs/arm/tris.md2"
    void $ modelIndex "models/objects/gibs/bone/tris.md2"
    void $ modelIndex "models/objects/gibs/bone2/tris.md2"
    void $ modelIndex "models/objects/gibs/chest/tris.md2"
    void $ modelIndex "models/objects/gibs/skull/tris.md2"
    void $ modelIndex "models/objects/gibs/head2/tris.md2"

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
  GenericEntThink "SP_func_wall" $ \edictReference -> do
    GameMisc.spFuncWall edictReference
    return True

spFuncObject :: EntThink
spFuncObject =
  GenericEntThink "SP_func_object" $ \edictReference -> do
    GameMisc.spFuncObject edictReference
    return True

spFuncTimer :: EntThink
spFuncTimer =
  GenericEntThink "SP_func_timer" $ \edictReference -> do
    GameFunc.spFuncTimer edictReference
    return True

spFuncExplosive :: EntThink
spFuncExplosive =
  GenericEntThink "SP_func_explosive" $ \edictReference -> do
    GameMisc.spFuncExplosive edictReference
    return True

spTriggerAlways :: EntThink
spTriggerAlways =
  GenericEntThink "SP_trigger_always" $ \edictReference -> do
    GameTrigger.spTriggerAlways edictReference
    return True

spTriggerOnce :: EntThink
spTriggerOnce =
  GenericEntThink "SP_trigger_once" $ \edictReference -> do
    GameTrigger.spTriggerOnce edictReference
    return True

spTriggerMultiple :: EntThink
spTriggerMultiple =
  GenericEntThink "SP_trigger_multiple" $ \edictReference -> do
    GameTrigger.spTriggerMultiple edictReference
    return True

spTriggerRelay :: EntThink
spTriggerRelay =
  GenericEntThink "SP_trigger_relay" $ \edictReference -> do
    GameTrigger.spTriggerRelay edictReference
    return True

spTriggerPush :: EntThink
spTriggerPush =
  GenericEntThink "SP_trigger_push" $ \edictReference -> do
    GameTrigger.spTriggerPush edictReference
    return True

spTriggerHurt :: EntThink
spTriggerHurt =
  GenericEntThink "SP_trigger_hurt" $ \edictReference -> do
    GameTrigger.spTriggerHurt edictReference
    return True

spTriggerKey :: EntThink
spTriggerKey =
  GenericEntThink "SP_trigger_key" $ \edictReference -> do
    GameTrigger.spTriggerKey edictReference
    return True

spTriggerCounter :: EntThink
spTriggerCounter =
  GenericEntThink "SP_trigger_counter" $ \edictReference -> do
    GameTrigger.spTriggerCounter edictReference
    return True

spTriggerGravity :: EntThink
spTriggerGravity =
  GenericEntThink "SP_trigger_gravity" $ \edictReference -> do
    GameTrigger.spTriggerGravity edictReference
    return True

spTriggerMonsterJump :: EntThink
spTriggerMonsterJump =
  GenericEntThink "SP_trigger_monsterjump" $ \edictReference -> do
    GameTrigger.spTriggerMonsterJump edictReference
    return True

spTargetTempEntity :: EntThink
spTargetTempEntity =
  GenericEntThink "SP_target_temp_entity" $ \edictReference -> do
    GameTarget.spTargetTempEntity edictReference
    return True

spTargetSpeaker :: EntThink
spTargetSpeaker =
  GenericEntThink "SP_target_speaker" $ \edictReference -> do
    GameTarget.spTargetSpeaker edictReference
    return True

spTargetExplosion :: EntThink
spTargetExplosion =
  GenericEntThink "SP_target_explosion" $ \edictReference -> do
    GameTarget.spTargetExplosion edictReference
    return True

spTargetChangeLevel :: EntThink
spTargetChangeLevel =
  GenericEntThink "SP_target_changelevel" $ \edictReference -> do
    GameTarget.spTargetChangeLevel edictReference
    return True

spTargetSecret :: EntThink
spTargetSecret =
  GenericEntThink "SP_target_secret" $ \edictReference -> do
    GameTarget.spTargetSecret edictReference
    return True

spTargetGoal :: EntThink
spTargetGoal =
  GenericEntThink "SP_target_goal" $ \edictReference -> do
    GameTarget.spTargetGoal edictReference
    return True


spTargetSplash :: EntThink
spTargetSplash =
  GenericEntThink "SP_target_splash" $ \edictReference -> do
    GameTarget.spTargetSplash edictReference
    return True

spTargetSpawner :: EntThink
spTargetSpawner =
  GenericEntThink "SP_target_spawner" $ \edictReference -> do
    GameTarget.spTargetSpawner edictReference
    return True

spTargetBlaster :: EntThink
spTargetBlaster =
  GenericEntThink "SP_target_blaster" $ \edictReference -> do
    GameTarget.spTargetBlaster edictReference
    return True

spTargetCrossLevelTrigger :: EntThink
spTargetCrossLevelTrigger =
  GenericEntThink "SP_target_crosslevel_trigger" $ \edictReference -> do
    GameTarget.spTargetCrossLevelTrigger edictReference
    return True

spTargetCrossLevelTarget :: EntThink
spTargetCrossLevelTarget =
  GenericEntThink "SP_target_crosslevel_target" $ \edictReference -> do
    GameTarget.spTargetCrossLevelTarget edictReference
    return True

spTargetLaser :: EntThink
spTargetLaser =
  GenericEntThink "SP_target_laser" $ \edictReference -> do
    GameTarget.spTargetLaser edictReference
    return True

spTargetHelp :: EntThink
spTargetHelp =
  GenericEntThink "SP_target_help" $ \edictReference -> do
    GameTarget.spTargetHelp edictReference
    return True

spTargetActor :: EntThink
spTargetActor =
  GenericEntThink "SP_target_actor" $ \edictReference -> do
    MActor.spTargetActor edictReference
    return True

spTargetLightRamp :: EntThink
spTargetLightRamp =
  GenericEntThink "SP_target_lightramp" $ \edictReference -> do
    GameTarget.spTargetLightRamp edictReference
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
