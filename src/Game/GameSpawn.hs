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
import Game.EntThink
import Game.SpawnT
import qualified Constants
import qualified Game.GameUtil as GameUtil
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
callSpawn er@(EdictReference idx) = do
    -- IMPROVE: does it apply to our code?
    -- if (null == ent.classname) {
    --     GameBase.gi.dprintf("ED_CallSpawn: null classname\n");
    --     return;
    -- }
    numItems <- use $ gameBaseGlobals.gbGame.glNumItems
    io (putStrLn "GameSpawn.callSpawn") >> undefined -- TODO

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
                    {-
            new spawn_t("func_wall", new EntThinkAdapter() {
        public String getID(){ return "func_wall"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_func_wall(ent);
                    return true;
                }
            }),
            new spawn_t("func_object", new EntThinkAdapter() {
        public String getID(){ return "SP_func_object"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_func_object(ent);
                    return true;
                }
            }),
            new spawn_t("func_timer", new EntThinkAdapter() {
        public String getID(){ return "SP_func_timer"; }
                public boolean think(edict_t ent) {
                    GameFunc.SP_func_timer(ent);
                    return true;
                }
            }),
            new spawn_t("func_explosive", new EntThinkAdapter() {
        public String getID(){ return "SP_func_explosive"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_func_explosive(ent);
                    return true;
                }
            }),
            new spawn_t("func_killbox", GameFunc.SP_func_killbox),
            new spawn_t("trigger_always", new EntThinkAdapter() {
        public String getID(){ return "SP_trigger_always"; }
                public boolean think(edict_t ent) {
                    GameTrigger.SP_trigger_always(ent);
                    return true;
                }
            }),
            new spawn_t("trigger_once", new EntThinkAdapter() {
        public String getID(){ return "SP_trigger_once"; }
                public boolean think(edict_t ent) {
                    GameTrigger.SP_trigger_once(ent);
                    return true;
                }
            }),
            new spawn_t("trigger_multiple", new EntThinkAdapter() {
        public String getID(){ return "SP_trigger_multiple"; }
                public boolean think(edict_t ent) {
                    GameTrigger.SP_trigger_multiple(ent);
                    return true;
                }
            }),
            new spawn_t("trigger_relay", new EntThinkAdapter() {
        public String getID(){ return "SP_trigger_relay"; }
                public boolean think(edict_t ent) {
                    GameTrigger.SP_trigger_relay(ent);
                    return true;
                }
            }),
            new spawn_t("trigger_push", new EntThinkAdapter() {
        public String getID(){ return "SP_trigger_push"; }
                
                public boolean think(edict_t ent) {
                    GameTrigger.SP_trigger_push(ent);
                    return true;
                }
            }),
            new spawn_t("trigger_hurt", new EntThinkAdapter() {
        public String getID(){ return "SP_trigger_hurt"; }
                public boolean think(edict_t ent) {
                    GameTrigger.SP_trigger_hurt(ent);
                    return true;
                }
            }),
            new spawn_t("trigger_key", new EntThinkAdapter() {
        public String getID(){ return "SP_trigger_key"; }
                public boolean think(edict_t ent) {
                    GameTrigger.SP_trigger_key(ent);
                    return true;
                }
            }),
            new spawn_t("trigger_counter", new EntThinkAdapter() {
        public String getID(){ return "SP_trigger_counter"; }
                public boolean think(edict_t ent) {
                    GameTrigger.SP_trigger_counter(ent);
                    return true;
                }
            }),
            new spawn_t("trigger_elevator", GameFunc.SP_trigger_elevator),
            new spawn_t("trigger_gravity", new EntThinkAdapter() {
        public String getID(){ return "SP_trigger_gravity"; }
                public boolean think(edict_t ent) {
                    GameTrigger.SP_trigger_gravity(ent);
                    return true;
                }
            }),
            new spawn_t("trigger_monsterjump", new EntThinkAdapter() {
        public String getID(){ return "SP_trigger_monsterjump"; }
                public boolean think(edict_t ent) {
                    GameTrigger.SP_trigger_monsterjump(ent);
                    return true;
                }
            }),
            new spawn_t("target_temp_entity", new EntThinkAdapter() {
        public String getID(){ return "SP_target_temp_entity"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_temp_entity(ent);
                    return true;
                }
            }),
            new spawn_t("target_speaker", new EntThinkAdapter() {
        public String getID(){ return "SP_target_speaker"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_speaker(ent);
                    return true;
                }
            }),
            new spawn_t("target_explosion", new EntThinkAdapter() {
        public String getID(){ return "SP_target_explosion"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_explosion(ent);
                    return true;
                }
            }),
            new spawn_t("target_changelevel", new EntThinkAdapter() {
        public String getID(){ return "SP_target_changelevel"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_changelevel(ent);
                    return true;
                }
            }),
            new spawn_t("target_secret", new EntThinkAdapter() {
        public String getID(){ return "SP_target_secret"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_secret(ent);
                    return true;
                }
            }),
            new spawn_t("target_goal", new EntThinkAdapter() {
        public String getID(){ return "SP_target_goal"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_goal(ent);
                    return true;
                }
            }),
            new spawn_t("target_splash", new EntThinkAdapter() {
        public String getID(){ return "SP_target_splash"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_splash(ent);
                    return true;
                }
            }),
            new spawn_t("target_spawner", new EntThinkAdapter() {
        public String getID(){ return "SP_target_spawner"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_spawner(ent);
                    return true;
                }
            }),
            new spawn_t("target_blaster", new EntThinkAdapter() {
        public String getID(){ return "SP_target_blaster"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_blaster(ent);
                    return true;
                }
            }),
            new spawn_t("target_crosslevel_trigger", new EntThinkAdapter() {
        public String getID(){ return "SP_target_crosslevel_trigger"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_crosslevel_trigger(ent);
                    return true;
                }
            }),
            new spawn_t("target_crosslevel_target", new EntThinkAdapter() {
        public String getID(){ return "SP_target_crosslevel_target"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_crosslevel_target(ent);
                    return true;
                }
            }),
            new spawn_t("target_laser", new EntThinkAdapter() {
        public String getID(){ return "SP_target_laser"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_laser(ent);
                    return true;
                }
            }),
            new spawn_t("target_help", new EntThinkAdapter() {
        public String getID(){ return "SP_target_help"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_help(ent);
                    return true;
                }
            }),
            new spawn_t("target_actor", new EntThinkAdapter() {
        public String getID(){ return "SP_target_actor"; }
                public boolean think(edict_t ent) {
                    M_Actor.SP_target_actor(ent);
                    return true;
                }
            }),
            new spawn_t("target_lightramp", new EntThinkAdapter() {
        public String getID(){ return "SP_target_lightramp"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_lightramp(ent);
                    return true;
                }
            }),
            new spawn_t("target_earthquake", new EntThinkAdapter() {
        public String getID(){ return "SP_target_earthquake"; }
                public boolean think(edict_t ent) {
                    GameTarget.SP_target_earthquake(ent);
                    return true;
                }
            }),
            new spawn_t("target_character", new EntThinkAdapter() {
        public String getID(){ return "SP_target_character"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_target_character(ent);
                    return true;
                }
            }),
            new spawn_t("target_string", new EntThinkAdapter() {
        public String getID(){ return "SP_target_string"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_target_string(ent);
                    return true;
                }
            }),
            new spawn_t("worldspawn", SP_worldspawn),
            new spawn_t("viewthing", new EntThinkAdapter() {
        public String getID(){ return "SP_viewthing"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_viewthing(ent);
                    return true;
                }
            }),
            new spawn_t("light", new EntThinkAdapter() {
        public String getID(){ return "SP_light"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_light(ent);
                    return true;
                }
            }),
            new spawn_t("light_mine1", new EntThinkAdapter() {
        public String getID(){ return "SP_light_mine1"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_light_mine1(ent);
                    return true;
                }
            }),
            new spawn_t("light_mine2", new EntThinkAdapter() {
        public String getID(){ return "SP_light_mine2"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_light_mine2(ent);
                    return true;
                }
            }),
            new spawn_t("info_null", new EntThinkAdapter() {
        public String getID(){ return "SP_info_null"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_info_null(ent);
                    return true;
                }
            }),
            new spawn_t("func_group", new EntThinkAdapter() {
        public String getID(){ return "SP_info_null"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_info_null(ent);
                    return true;
                }
            }),
            new spawn_t("info_notnull", new EntThinkAdapter() {
        public String getID(){ return "info_notnull"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_info_notnull(ent);
                    return true;
                }
            }),
            new spawn_t("path_corner", new EntThinkAdapter() {
        public String getID(){ return "SP_path_corner"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_path_corner(ent);
                    return true;
                }
            }),
            new spawn_t("point_combat", new EntThinkAdapter() {
        public String getID(){ return "SP_point_combat"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_point_combat(ent);
                    return true;
                }
            }),
            new spawn_t("misc_explobox", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_explobox"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_explobox(ent);
                    return true;
                }
            }),
            new spawn_t("misc_banner", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_banner"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_banner(ent);
                    return true;
                }
            }),
            new spawn_t("misc_satellite_dish", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_satellite_dish"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_satellite_dish(ent);
                    return true;
                }
            }),
            new spawn_t("misc_actor", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_actor"; }
                public boolean think(edict_t ent) {
                    M_Actor.SP_misc_actor(ent);
                    return false;
                }
            }),
            new spawn_t("misc_gib_arm", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_gib_arm"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_gib_arm(ent);
                    return true;
                }
            }),
            new spawn_t("misc_gib_leg", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_gib_leg"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_gib_leg(ent);
                    return true;
                }
            }),
            new spawn_t("misc_gib_head", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_gib_head"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_gib_head(ent);
                    return true;
                }
            }),
            new spawn_t("misc_insane", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_insane"; }
                public boolean think(edict_t ent) {
                    M_Insane.SP_misc_insane(ent);
                    return true;
                }
            }),
            new spawn_t("misc_deadsoldier", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_deadsoldier"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_deadsoldier(ent);
                    return true;
                }
            }),
            new spawn_t("misc_viper", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_viper"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_viper(ent);
                    return true;
                }
            }),
            new spawn_t("misc_viper_bomb", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_viper_bomb"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_viper_bomb(ent);
                    return true;
                }
            }),
            new spawn_t("misc_bigviper", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_bigviper"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_bigviper(ent);
                    return true;
                }
            }),
            new spawn_t("misc_strogg_ship", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_strogg_ship"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_strogg_ship(ent);
                    return true;
                }
            }),
            new spawn_t("misc_teleporter", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_teleporter"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_teleporter(ent);
                    return true;
                }
            }),
            new spawn_t("misc_teleporter_dest",
                    GameMisc.SP_misc_teleporter_dest),
            new spawn_t("misc_blackhole", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_blackhole"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_blackhole(ent);
                    return true;
                }
            }),
            new spawn_t("misc_eastertank", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_eastertank"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_eastertank(ent);
                    return true;
                }
            }),
            new spawn_t("misc_easterchick", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_easterchick"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_easterchick(ent);
                    return true;
                }
            }),
            new spawn_t("misc_easterchick2", new EntThinkAdapter() {
        public String getID(){ return "SP_misc_easterchick2"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_misc_easterchick2(ent);
                    return true;
                }
            }),
            new spawn_t("monster_berserk", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_berserk"; }
                public boolean think(edict_t ent) {
                    M_Berserk.SP_monster_berserk(ent);
                    return true;
                }
            }),
            new spawn_t("monster_gladiator", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_gladiator"; }
                public boolean think(edict_t ent) {
                    M_Gladiator.SP_monster_gladiator(ent);
                    return true;
                }
            }),
            new spawn_t("monster_gunner", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_gunner"; }
                public boolean think(edict_t ent) {
                    M_Gunner.SP_monster_gunner(ent);
                    return true;
                }
            }),
            new spawn_t("monster_infantry", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_infantry"; }
                public boolean think(edict_t ent) {
                    M_Infantry.SP_monster_infantry(ent);
                    return true;
                }
            }),
            new spawn_t("monster_soldier_light",
                    M_Soldier.SP_monster_soldier_light),
            new spawn_t("monster_soldier", M_Soldier.SP_monster_soldier),
            new spawn_t("monster_soldier_ss", M_Soldier.SP_monster_soldier_ss),
            new spawn_t("monster_tank", M_Tank.SP_monster_tank),
            new spawn_t("monster_tank_commander", M_Tank.SP_monster_tank),
            new spawn_t("monster_medic", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_medic"; }
                public boolean think(edict_t ent) {
                    M_Medic.SP_monster_medic(ent);
                    return true;
                }
            }), new spawn_t("monster_flipper", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_flipper"; }
                public boolean think(edict_t ent) {
                    M_Flipper.SP_monster_flipper(ent);
                    return true;
                }
            }), new spawn_t("monster_chick", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_chick"; }
                public boolean think(edict_t ent) {
                    M_Chick.SP_monster_chick(ent);
                    return true;
                }
            }),
            new spawn_t("monster_parasite", M_Parasite.SP_monster_parasite),
            new spawn_t("monster_flyer", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_flyer"; }
                public boolean think(edict_t ent) {
                    M_Flyer.SP_monster_flyer(ent);
                    return true;
                }
            }), new spawn_t("monster_brain", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_brain"; }
                public boolean think(edict_t ent) {
                    M_Brain.SP_monster_brain(ent);
                    return true;
                }
            }), new spawn_t("monster_floater", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_floater"; }
                public boolean think(edict_t ent) {
                    M_Float.SP_monster_floater(ent);
                    return true;
                }
            }), new spawn_t("monster_hover", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_hover"; }
                public boolean think(edict_t ent) {
                    M_Hover.SP_monster_hover(ent);
                    return true;
                }
            }), new spawn_t("monster_mutant", M_Mutant.SP_monster_mutant),
            new spawn_t("monster_supertank", M_Supertank.SP_monster_supertank),
            new spawn_t("monster_boss2", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_boss2"; }
                public boolean think(edict_t ent) {
                    M_Boss2.SP_monster_boss2(ent);
                    return true;
                }
            }), new spawn_t("monster_boss3_stand", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_boss3_stand"; }
                public boolean think(edict_t ent) {
                    M_Boss3.SP_monster_boss3_stand(ent);
                    return true;
                }
            }), new spawn_t("monster_jorg", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_jorg"; }
                public boolean think(edict_t ent) {
                    M_Boss31.SP_monster_jorg(ent);
                    return true;
                }
            }), new spawn_t("monster_commander_body", new EntThinkAdapter() {
        public String getID(){ return "SP_monster_commander_body"; }
                public boolean think(edict_t ent) {
                    GameMisc.SP_monster_commander_body(ent);
                    return true;
                }
            }), new spawn_t("turret_breach", new EntThinkAdapter() {
        public String getID(){ return "SP_turret_breach"; }
                public boolean think(edict_t ent) {
                    GameTurret.SP_turret_breach(ent);
                    return true;
                }
            }), new spawn_t("turret_base", new EntThinkAdapter() {
        public String getID(){ return "SP_turret_base"; }
                public boolean think(edict_t ent) {
                    GameTurret.SP_turret_base(ent);
                    return true;
                }
            }), new spawn_t("turret_driver", new EntThinkAdapter() {
        public String getID(){ return "SP_turret_driver"; }
                public boolean think(edict_t ent) {
                    GameTurret.SP_turret_driver(ent);
                    return true;
                }
            }), new spawn_t(null, null) };
            -}
                    ]

spItemHealth :: EntThink
spItemHealth =
  GenericEntThink "SP_item_health" $ \edictReference -> do
    GameItem.spItemHealth edictReference
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
  GenericEntThink "SP_worldspawn" $ \edictReference -> do
    io (putStrLn "GameSpawn.spWorldSpawn") >> undefined -- TODO

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


