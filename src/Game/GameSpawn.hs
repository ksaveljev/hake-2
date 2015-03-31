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
