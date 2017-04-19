module Game.GameSpawn
    ( callSpawn
    , spawnEntities
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (%~))
import           Control.Monad         (void, when, unless)
import           Data.Bits             (complement, (.&.), (.|.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector           as V
import           Data.Char             (toLower)
import           Linear                (V3(..))

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameFunc            as GameFunc
import {-# SOURCE #-} Game.GameImportT
import qualified Game.GameItems           as GameItems
import           Game.GameLocalsT 
import qualified Game.GameMisc            as GameMisc
import qualified Game.GameTarget          as GameTarget
import qualified Game.GameTrigger         as GameTrigger
import qualified Game.GameTurret          as GameTurret
import qualified Game.GameUtil            as GameUtil
import           Game.GItemT
import           Game.LevelLocalsT
import qualified Game.Monsters.MActor     as MActor
import qualified Game.Monsters.MBerserk   as MBerserk
import qualified Game.Monsters.MBoss2     as MBoss2
import qualified Game.Monsters.MBoss3     as MBoss3
import qualified Game.Monsters.MBoss31    as MBoss31
import qualified Game.Monsters.MBrain     as MBrain
import qualified Game.Monsters.MChick     as MChick
import qualified Game.Monsters.MFlipper   as MFlipper
import qualified Game.Monsters.MFloat     as MFloat
import qualified Game.Monsters.MFlyer     as MFlyer
import qualified Game.Monsters.MGladiator as MGladiator
import qualified Game.Monsters.MGunner    as MGunner
import qualified Game.Monsters.MHover     as MHover
import qualified Game.Monsters.MInfantry  as MInfantry
import qualified Game.Monsters.MInsane    as MInsane
import qualified Game.Monsters.MMedic     as MMedic
import qualified Game.Monsters.MMutant    as MMutant
import qualified Game.Monsters.MParasite  as MParasite
import qualified Game.Monsters.MSoldier   as MSoldier
import qualified Game.Monsters.MSuperTank as MSuperTank
import qualified Game.Monsters.MTank      as MTank
import qualified Game.PlayerClient        as PlayerClient
import qualified Game.PlayerTrail         as PlayerTrail
import           Game.SpawnT
import           Game.SpawnTempT
import qualified QCommon.Com              as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary              (encode)
import qualified Util.Lib                 as Lib

callSpawn :: Ref EdictT -> Quake ()
callSpawn edictRef = do
    numItems <- use (gameBaseGlobals.gbGame.glNumItems)
    edict <- readRef edictRef
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
              Just idx -> void (entThink ((spawns V.! idx)^.spSpawn) edictRef)
              Nothing -> do
                  dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
                  dprintf (edictClassName `B.append` " doesn't have a spawn function\n")
  where 
    checkItemSpawn edictClassName idx maxIdx
        | idx == maxIdx = return Nothing
        | otherwise = do
            item <- readRef (Ref idx)
            if edictClassName == BC.map toLower (item^.giClassName)
                then return (Just (Ref idx))
                else checkItemSpawn edictClassName (idx + 1) maxIdx

spawnEntities :: B.ByteString -> B.ByteString -> B.ByteString -> Quake ()
spawnEntities mapName entities spawnPoint = do
    Com.dprintf (B.concat ["SpawnEntities(), mapname=", mapName, "\n"])
    setSkill =<< skillCVar
    PlayerClient.saveClientData
    gameBaseGlobals.gbLevel .= newLevelLocalsT
    gameBaseGlobals.gbLevel.llMapName .= mapName
    gameBaseGlobals.gbGame.glSpawnPoint .= spawnPoint
    initEntities
    initClients
    parseAndSpawnEntities entities mapName
    findTeams
    PlayerTrail.initialize

setSkill :: CVarT -> Quake ()
setSkill skill =
    when (skillValue /= skillLevel) $ do
        forceSet <- use (gameBaseGlobals.gbGameImport.giCVarForceSet)
        void (forceSet "skill" (encode skillLevel))
        Com.dprintf (B.concat ["player skill level:", encode skillValue, "\n"])
  where
    skillValue = fromIntegral (floor (skill^.cvValue) :: Int) :: Float
    skillLevel | skillValue < 0 = 0
               | skillValue > 3 = 3
               | otherwise = skillValue

initEntities :: Quake ()
initEntities = do
    maxEntities <- use (gameBaseGlobals.gbGame.glMaxEntities)
    mapM_ (\idx -> writeRef (Ref idx) (newEdictT idx)) [0..maxEntities-1]

initClients :: Quake ()
initClients = do
    maxClients <- use (gameBaseGlobals.gbGame.glMaxClients)
    mapM_ (\idx -> modifyRef (Ref idx) (\v -> v & eClient .~ Just (Ref (idx - 1)))) [1..maxClients]

parseAndSpawnEntities :: B.ByteString -> B.ByteString -> Quake ()
parseAndSpawnEntities entities mapName = do
    inhibited <- parseEntities entities mapName True 0 0
    Com.dprintf (encode inhibited `B.append` " entities inhibited.\n")

parseEntities :: B.ByteString -> B.ByteString -> Bool -> Int -> Int -> Quake Int
parseEntities entities mapName initial idx inhibited
    | idx >= B.length entities = return inhibited -- RESEARCH: make sure this is correct? is this present in jake2?
    | otherwise = do
        (token, updatedIdx) <- Com.parse entities (B.length entities) idx
        maybe (return inhibited) (processToken entities mapName initial updatedIdx inhibited) token

processToken :: B.ByteString -> B.ByteString -> Bool -> Int -> Int -> B.ByteString -> Quake Int
processToken entities mapName initial idx inhibited token = do
    checkError
    edictRef <- spawnEdict initial
    updatedIdx <- parseEdict edictRef entities idx
    printEdictInfo edictRef
    yetAnotherMapHack edictRef mapName
    removed <- removeThingsBasedOnSkillAndDeathmatch edictRef
    finishSpawn entities mapName updatedIdx inhibited edictRef removed
  where
    checkError = when (BC.head token /= '{') $ do
        err <- use (gameBaseGlobals.gbGameImport.giError)
        err (B.concat ["ED_LoadFromFile: found ", token, " when expecting {"])

spawnEdict :: Bool -> Quake (Ref EdictT)
spawnEdict initial
    | initial = return worldRef
    | otherwise = GameUtil.spawn

printEdictInfo :: Ref EdictT -> Quake ()
printEdictInfo edictRef = do
    edict <- readRef edictRef
    Com.dprintf (B.concat [ "spawning ent[", encode (edict^.eIndex)
                          , "], classname=", edict^.eClassName
                          , ", flags=", encode (edict^.eSpawnFlags)
                          ])

yetAnotherMapHack :: Ref EdictT -> B.ByteString -> Quake ()
yetAnotherMapHack edictRef mapName = do
    edict <- readRef edictRef
    when (shouldApplyHack edict (edict^.eiModel)) $
        modifyRef edictRef (\v -> v & eSpawnFlags %~ (.&. complement Constants.spawnFlagNotHard))
  where
    shouldApplyHack _ Nothing = False
    shouldApplyHack edict (Just model) =
        BC.map toLower mapName == "command" &&
        BC.map toLower (edict^.eClassName) == "trigger_once" &&
        BC.map toLower model == "*27"

removeThingsBasedOnSkillAndDeathmatch :: Ref EdictT -> Quake Bool
removeThingsBasedOnSkillAndDeathmatch edictRef
    | edictRef == worldRef = return False
    | otherwise = do
        edict <- readRef edictRef
        freed <- checkDeathmatchAndSkill edictRef edict =<< deathmatchCVar
        checkFreed edictRef freed

checkDeathmatchAndSkill :: Ref EdictT -> EdictT -> CVarT -> Quake Bool
checkDeathmatchAndSkill edictRef edict deathmatch
    | isDeathmatch && (edict^.eSpawnFlags) .&. Constants.spawnFlagNotDeathmatch /= 0 =
        inhibitEdict edictRef
    | isDeathmatch = return False
    | otherwise = checkSkill edictRef edict =<< skillCVar
  where
    isDeathmatch = (deathmatch^.cvValue) /= 0

checkSkill :: Ref EdictT -> EdictT -> CVarT -> Quake Bool
checkSkill edictRef edict skill
    | wrongSkillLevel = inhibitEdict edictRef
    | otherwise = return False
  where
    wrongSkillLevel =
        ((skill^.cvValue) == 0 && (edict^.eSpawnFlags .&. Constants.spawnFlagNotEasy) /= 0) ||
        ((skill^.cvValue) == 1 && (edict^.eSpawnFlags .&. Constants.spawnFlagNotMedium) /= 0) ||
        (((skill^.cvValue) == 2 || (skill^.cvValue) == 3) && (edict^.eSpawnFlags .&. Constants.spawnFlagNotHard) /= 0)

inhibitEdict :: Ref EdictT -> Quake Bool
inhibitEdict edictRef = do
    Com.dprintf "->inhibited.\n"
    GameUtil.freeEdict edictRef
    return True

checkFreed :: Ref EdictT -> Bool -> Quake Bool
checkFreed edictRef freed
    | freed = do
        modifyRef edictRef (\v -> v & eSpawnFlags %~ (.&. complement flags))
        return True
    | otherwise = return False
  where
    flags = Constants.spawnFlagNotEasy .|. Constants.spawnFlagNotMedium .|.
            Constants.spawnFlagNotHard .|. Constants.spawnFlagNotCoop .|.
            Constants.spawnFlagNotDeathmatch

finishSpawn :: B.ByteString -> B.ByteString -> Int -> Int -> Ref EdictT -> Bool -> Quake Int
finishSpawn entities mapName idx inhibited edictRef removed
    | removed = parseEntities entities mapName False idx (inhibited + 1)
    | otherwise = do
        callSpawn edictRef
        Com.dprintf "\n"
        parseEntities entities mapName False idx inhibited

findTeams :: Quake ()
findTeams = do
    numEdicts <- use (gameBaseGlobals.gbNumEdicts)
    (teamsNum, entitiesNum) <- findNextTeam 0 0 1 numEdicts
    dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
    dprintf (B.concat [encode teamsNum, " teams with ", encode entitiesNum, " entities\n"])

findNextTeam :: Int -> Int -> Int -> Int -> Quake (Int, Int)
findNextTeam c c2 idx maxIdx
    | idx >= maxIdx = return (c, c2)
    | otherwise = do
        edict <- readRef (Ref idx)
        proceedFindNextTeam (Ref idx) edict (edict^.eTeam)
  where
    proceedFindNextTeam _ _ Nothing = findNextTeam c c2 (idx + 1) maxIdx
    proceedFindNextTeam edictRef edict (Just team)
        | not (edict^.eInUse) || (edict^.eFlags) .&. Constants.flTeamSlave /= 0 =
            findNextTeam c c2 (idx + 1) maxIdx
        | otherwise = do
            modifyRef edictRef (\v ->v & eTeamMaster .~ Just edictRef)
            c2' <- findTeamMembers team edictRef edictRef idx maxIdx c2
            findNextTeam (c + 1) c2' (idx + 1) maxIdx

findTeamMembers :: B.ByteString -> Ref EdictT -> Ref EdictT -> Int -> Int -> Int -> Quake Int
findTeamMembers teamName master chainRef idx maxIdx c2
    | idx >= maxIdx = return c2
    | otherwise = do
        edict <- readRef (Ref idx)
        proceedFindTeamMembers (Ref idx) edict (edict^.eTeam)
  where
    proceedFindTeamMembers _ _ Nothing = findTeamMembers teamName master chainRef (idx + 1) maxIdx c2
    proceedFindTeamMembers edictRef edict (Just team)
        | not (edict^.eInUse) || (edict^.eFlags) .&. Constants.flTeamSlave /= 0 || teamName /= team =
            findTeamMembers teamName master chainRef (idx + 1) maxIdx c2
        | otherwise = do
            modifyRef chainRef (\v -> v & eTeamChain .~ Just edictRef)
            modifyRef edictRef (\v -> v & eTeamMaster .~ Just master
                                        & eFlags %~ (.|. Constants.flTeamSlave))
            findTeamMembers teamName master edictRef (idx + 1) maxIdx (c2 + 1)

parseEdict :: Ref EdictT -> B.ByteString -> Int -> Quake Int
parseEdict edictRef entities idx = do
    gameBaseGlobals.gbSpawnTemp .= newSpawnTempT
    (newIdx, initial) <- parse False idx
    unless initial $
        GameUtil.clearEdict edictRef
    return newIdx
  where
    parse initial index = do
        (comToken, newIdx) <- Com.parse entities (B.length entities) index
        maybe (parseError initial newIdx) (proceedParse initial newIdx) comToken
    parseError initial newIdx = do
        err <- use (gameBaseGlobals.gbGameImport.giError)
        err "ED_ParseEntity: EOF without closing brace"
        return (newIdx, initial)
    proceedParse initial newIdx token
        | token == "}" = return (newIdx, initial)
        | otherwise = do
            (anotherComToken, finalIdx) <- Com.parse entities (B.length entities) newIdx
            maybe (parseError2 initial finalIdx) (proceedParse2 initial finalIdx token) anotherComToken
    parseError2 initial finalIdx = do
        err <- use (gameBaseGlobals.gbGameImport.giError)
        err "ED_ParseEntity: EOF without closing brace"
        return (finalIdx, initial)
    proceedParse2 _ finalIdx token anotherToken = do
        when (anotherToken == "}") $ do
            err <- use (gameBaseGlobals.gbGameImport.giError)
            err "ED_ParseEntity: closing brace without data"
        -- keynames with a leading underscore are used for utility comments,
        -- and are immediately discarded by quake
        if BC.head token == '_'
            then
                parse True finalIdx
            else do
                parseField (BC.map toLower token) anotherToken edictRef
                parse True finalIdx

parseField :: B.ByteString -> B.ByteString -> Ref EdictT -> Quake ()
parseField key value edictRef = do
    when (key == "nextmap") $
        Com.println ("nextmap: " `B.append` value)
    spawnTemp <- use (gameBaseGlobals.gbSpawnTemp)
    let (updatedSpawnTemp, updated) = setSpawnTempField spawnTemp key value
    proceedParseField updatedSpawnTemp updated
  where
    proceedParseField updatedSpawnTemp updated
        | updated = gameBaseGlobals.gbSpawnTemp .= updatedSpawnTemp
        | otherwise = do
            edict <- readRef edictRef
            let (updatedEdict, updated') = setEdictField edict key value
            doParseField updatedEdict updated'
    doParseField updatedEdict updated
        | updated = writeRef edictRef updatedEdict
        | otherwise = do
            dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
            dprintf (B.concat ["??? The key [", key, "] is not a field\n"])

setSpawnTempField :: SpawnTempT -> B.ByteString -> B.ByteString -> (SpawnTempT, Bool)
setSpawnTempField spawnTemp key value =
    case key of
        "lip"       -> (spawnTemp & stLip       .~ Lib.atoi         value , True)
        "distance"  -> (spawnTemp & stDistance  .~ Lib.atoi         value , True)
        "height"    -> (spawnTemp & stHeight    .~ Lib.atoi         value , True)
        "noise"     -> (spawnTemp & stNoise     .~ Just (newString  value), True)
        "pausetime" -> (spawnTemp & stPauseTime .~ Lib.atof         value , True)
        "item"      -> (spawnTemp & stItem      .~ Just (newString  value), True)
        "gravity"   -> (spawnTemp & stGravity   .~ Just (newString  value), True)
        "sky"       -> (spawnTemp & stSky       .~ newString        value , True)
        "skyrotate" -> (spawnTemp & stSkyRotate .~ Lib.atof         value , True)
        "skyaxis"   -> (spawnTemp & stSkyAxis   .~ Lib.atov         value , True)
        "minyaw"    -> (spawnTemp & stMinYaw    .~ Lib.atof         value , True)
        "maxyaw"    -> (spawnTemp & stMaxYaw    .~ Lib.atof         value , True)
        "minpitch"  -> (spawnTemp & stMinPitch  .~ Lib.atof         value , True)
        "maxpitch"  -> (spawnTemp & stMaxPitch  .~ Lib.atof         value , True)
        "nextmap"   -> (spawnTemp & stNextMap   .~ newString        value , True)
        _           -> (spawnTemp, False)

setEdictField :: EdictT -> B.ByteString -> B.ByteString -> (EdictT, Bool)
setEdictField e key value =
    case key of
        "classname"    -> (e & eClassName            .~ newString value       , True)
        "model"        -> (e & eiModel               .~ Just (newString value), True)
        "spawnflags"   -> (e & eSpawnFlags           .~ Lib.atoi value        , True)
        "speed"        -> (e & eSpeed                .~ Lib.atof value        , True)
        "accel"        -> (e & eAccel                .~ Lib.atof value        , True)
        "decel"        -> (e & eDecel                .~ Lib.atof value        , True)
        "target"       -> (e & eTarget               .~ Just (newString value), True)
        "targetname"   -> (e & eTargetName           .~ Just (newString value), True)
        "pathtarget"   -> (e & ePathTarget           .~ Just (newString value), True)
        "deathtarget"  -> (e & eDeathTarget          .~ Just (newString value), True)
        "killtarget"   -> (e & eKillTarget           .~ Just (newString value), True)
        "combattarget" -> (e & eCombatTarget         .~ Just (newString value), True)
        "message"      -> (e & eMessage              .~ Just (newString value), True)
        "team"         -> (e & eTeam                 .~ Just (newString value), True) -- TODO: we need to call Com.dprintf here
        "wait"         -> (e & eWait                 .~ Lib.atof value        , True)
        "delay"        -> (e & eDelay                .~ Lib.atof value        , True)
        "random"       -> (e & eRandom               .~ Lib.atof value        , True)
        "move_origin"  -> (e & eMoveOrigin           .~ Lib.atov value        , True)
        "move_angles"  -> (e & eMoveAngles           .~ Lib.atov value        , True)
        "style"        -> (e & eStyle                .~ Lib.atoi value        , True)
        "count"        -> (e & eCount                .~ Lib.atoi value        , True)
        "health"       -> (e & eHealth               .~ Lib.atoi value        , True)
        "sounds"       -> (e & eSounds               .~ Lib.atoi value        , True)
        "dmg"          -> (e & eDmg                  .~ Lib.atoi value        , True)
        "mass"         -> (e & eMass                 .~ Lib.atoi value        , True)
        "volume"       -> (e & eVolume               .~ Lib.atof value        , True)
        "attenuation"  -> (e & eAttenuation          .~ Lib.atof value        , True)
        "map"          -> (e & eMap                  .~ Just (newString value), True)
        "origin"       -> (e & eEntityState.esOrigin .~ Lib.atov value        , True)
        "angles"       -> (e & eEntityState.esAngles .~ Lib.atov value        , True)
        "angle"        -> (e & eEntityState.esAngles .~ V3 0 (Lib.atof value) 0, True)
        "light"        -> (e, True)
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

spawns :: V.Vector SpawnT
spawns = V.fromList
    [ SpawnT "item_health" spItemHealth
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
spItemHealth = EntThink "SP_item_health" $ \edictRef -> do
    GameItems.spItemHealth edictRef
    return True

spItemHealthSmall :: EntThink
spItemHealthSmall = EntThink "SP_item_health_small" $ \edictRef -> do
    GameItems.spItemHealthSmall edictRef
    return True

spItemHealthLarge :: EntThink
spItemHealthLarge = EntThink "SP_item_health_large" $ \edictRef -> do
    GameItems.spItemHealthLarge edictRef
    return True

spItemHealthMega :: EntThink
spItemHealthMega = EntThink "SP_item_health_mega" $ \edictRef -> do
    GameItems.spItemHealthMega edictRef
    return True

spInfoPlayerStart :: EntThink
spInfoPlayerStart = EntThink "SP_info_player_start" $ \edictRef -> do
    PlayerClient.spInfoPlayerStart edictRef
    return True

spInfoPlayerDeathmatch :: EntThink
spInfoPlayerDeathmatch = EntThink "SP_info_player_deathmatch" $ \edictRef -> do
    PlayerClient.spInfoPlayerDeathmatch edictRef
    return True

spInfoPlayerCoop :: EntThink
spInfoPlayerCoop = EntThink "SP_info_player_coop" $ \edictRef -> do
    PlayerClient.spInfoPlayerCoop edictRef
    return True

spInfoPlayerIntermission :: EntThink
spInfoPlayerIntermission = EntThink "SP_info_player_intermission" $ \_ -> do
    PlayerClient.spInfoPlayerIntermission
    return True

spFuncPlat :: EntThink
spFuncPlat = EntThink "SP_func_plat" $ \edictRef -> do
    GameFunc.spFuncPlat edictRef
    return True

spFuncWater :: EntThink
spFuncWater = EntThink "SP_func_water" $ \edictRef -> do
    GameFunc.spFuncWater edictRef
    return True

spFuncTrain :: EntThink
spFuncTrain = EntThink "SP_func_train" $ \edictRef -> do
    GameFunc.spFuncTrain edictRef
    return True

spFuncClock :: EntThink
spFuncClock = EntThink "SP_func_clock" $ \edictRef -> do
    GameMisc.spFuncClock edictRef
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
spWorldSpawn = EntThink "SP_worldspawn" $ \edictRef -> do
    edict <- readRef edictRef
    modifyRef edictRef (\v -> v & eMoveType .~ Constants.moveTypePush
                                & eSolid .~ Constants.solidBsp
                                -- since the world doesn't use G_Spawn()
                                & eInUse .~ True
                                -- world model is always index 1
                                & eEntityState.esModelIndex .~ 1)
    -- reserve some spots for dead player bodies for coop / deathmatch
    PlayerClient.initBodyQue
    -- set configstrings for items
    GameItems.setItemNames
    nextMap <- use (gameBaseGlobals.gbSpawnTemp.stNextMap)
    gameBaseGlobals.gbLevel.llNextMap .= nextMap
    -- make some data visible to the server
    gameImport <- use (gameBaseGlobals.gbGameImport)
    let configString = gameImport^.giConfigString
        imageIndex = gameImport^.giImageIndex
        soundIndex = gameImport^.giSoundIndex
        modelIndex = gameImport^.giModelIndex
        cvarSet = gameImport^.giCVarSet
    setLevelName configString (edict^.eMessage)
    spawnTemp <- use (gameBaseGlobals.gbSpawnTemp)
    configString Constants.csSky (if B.length (spawnTemp^.stSky) > 0 then spawnTemp^.stSky else "unit1_")
    configString Constants.csSkyRotate (encode (spawnTemp^.stSkyRotate))
    configString Constants.csSkyAxis (Lib.vtos (spawnTemp^.stSkyAxis))
    configString Constants.csCdTrack (encode (edict^.eSounds))
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar :: Quake Int
    configString Constants.csMaxClients (encode maxClients)
    -- status bar program
    deathmatch <- fmap (^.cvValue) deathmatchCVar
    configString Constants.csStatusBar (if deathmatch /= 0 then dmStatusBar else singleStatusBar)
    -- help icon for statusbar
    void (imageIndex (Just "i_help"))
    imageIndex (Just "i_health") >>= (gameBaseGlobals.gbLevel.llPicHealth .=)
    void (imageIndex (Just "help"))
    void (imageIndex (Just "field_3"))
    void (setGravity cvarSet (spawnTemp^.stGravity))
    soundIndex (Just "player/fry.wav") >>= (gameBaseGlobals.gbSndFry .=)
    -- standing in lava / slime
    GameItems.findItem "Blaster" >>= GameItems.precacheItem
    void (soundIndex (Just "player/lava1.wav"))
    void (soundIndex (Just "player/lava2.wav"))
    void (soundIndex (Just "misc/pc_up.wav"))
    void (soundIndex (Just "misc/talk1.wav"))
    void (soundIndex (Just "misc/udeath.wav"))
    -- gibs
    void (soundIndex (Just "items/respawn1.wav"))
    -- sexed sounds
    void (soundIndex (Just "*death1.wav"))
    void (soundIndex (Just "*death2.wav"))
    void (soundIndex (Just "*death3.wav"))
    void (soundIndex (Just "*death4.wav"))
    void (soundIndex (Just "*fall1.wav"))
    void (soundIndex (Just "*fall2.wav"))
    void (soundIndex (Just "*gurp1.wav"))
    -- drowning damage
    void (soundIndex (Just "*gurp2.wav"))
    void (soundIndex (Just "*jump1.wav"))
    -- player jump
    void (soundIndex (Just "*pain25_1.wav"))
    void (soundIndex (Just "*pain25_2.wav"))
    void (soundIndex (Just "*pain50_1.wav"))
    void (soundIndex (Just "*pain50_2.wav"))
    void (soundIndex (Just "*pain75_1.wav"))
    void (soundIndex (Just "*pain75_2.wav"))
    void (soundIndex (Just "*pain100_1.wav"))
    void (soundIndex (Just "*pain100_2.wav"))
    -- sexed models
    -- THIS ORDER MUST MATCH THE DEFINES IN g_local.h
    -- you can add more, max 15
    void (modelIndex (Just "#w_blaster.md2"))
    void (modelIndex (Just "#w_shotgun.md2"))
    void (modelIndex (Just "#w_sshotgun.md2"))
    void (modelIndex (Just "#w_machinegun.md2"))
    void (modelIndex (Just "#w_chaingun.md2"))
    void (modelIndex (Just "#a_grenades.md2"))
    void (modelIndex (Just "#w_glauncher.md2"))
    void (modelIndex (Just "#w_rlauncher.md2"))
    void (modelIndex (Just "#w_hyperblaster.md2"))
    void (modelIndex (Just "#w_railgun.md2"))
    void (modelIndex (Just "#w_bfg.md2"))
    -- --------------
    void (soundIndex (Just "player/gasp1.wav"))
    -- gasping for air
    void (soundIndex (Just "player/gasp2.wav"))
    -- head breaking surface, not gasping
    void (soundIndex (Just "player/watr_in.wav"))
    -- feet hitting water
    void (soundIndex (Just "player/watr_out.wav"))
    -- feet leaving water
    void (soundIndex (Just "player/watr_un.wav"))
    -- head going underwater
    void (soundIndex (Just "player/u_breath1.wav"))
    void (soundIndex (Just "player/u_breath2.wav"))
    void (soundIndex (Just "items/pkup.wav"))
    -- bonus item pickup
    void (soundIndex (Just "world/land.wav"))
    -- landing thud
    void (soundIndex (Just "misc/h2ohit1.wav"))
    -- landing splash
    void (soundIndex (Just "items/damage.wav"))
    void (soundIndex (Just "items/protect.wav"))
    void (soundIndex (Just "items/protect4.wav"))
    void (soundIndex (Just "weapons/noammo.wav"))
    void (soundIndex (Just "infantry/inflies1.wav"))
    modelIndex (Just "models/objects/gibs/sm_meat/tris.md2") >>= (gameBaseGlobals.gbSmMeatIndex .=)
    void (modelIndex (Just "models/objects/gibs/arm/tris.md2"))
    void (modelIndex (Just "models/objects/gibs/bone/tris.md2"))
    void (modelIndex (Just "models/objects/gibs/bone2/tris.md2"))
    void (modelIndex (Just "models/objects/gibs/chest/tris.md2"))
    void (modelIndex (Just "models/objects/gibs/skull/tris.md2"))
    void (modelIndex (Just "models/objects/gibs/head2/tris.md2"))
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
  where
    setLevelName configString (Just msg)
        | B.length msg > 0 = do
            void (configString Constants.csName msg)
            gameBaseGlobals.gbLevel.llLevelName .= msg
        | otherwise = setDefaultName
    setLevelName _ Nothing = setDefaultName
    setDefaultName = do
        mapName <- use (gameBaseGlobals.gbLevel.llMapName)
        gameBaseGlobals.gbLevel.llLevelName .= mapName
    setGravity cvarSet Nothing = cvarSet "sv_gravity" "800"
    setGravity cvarSet (Just gravity)
        | gravity == B.empty = cvarSet "sv_gravity" "800"
        | otherwise          = cvarSet "sv_gravity" gravity

spFuncWall :: EntThink
spFuncWall = EntThink "SP_func_wall" $ \edictRef -> do
    GameMisc.spFuncWall edictRef
    return True

spFuncObject :: EntThink
spFuncObject = EntThink "SP_func_object" $ \edictRef -> do
    GameMisc.spFuncObject edictRef
    return True

spFuncTimer :: EntThink
spFuncTimer = EntThink "SP_func_timer" $ \edictRef -> do
    GameFunc.spFuncTimer edictRef
    return True

spFuncExplosive :: EntThink
spFuncExplosive = EntThink "SP_func_explosive" $ \edictRef -> do
    GameMisc.spFuncExplosive edictRef
    return True

spTriggerAlways :: EntThink
spTriggerAlways = EntThink "SP_trigger_always" $ \edictRef -> do
    GameTrigger.spTriggerAlways edictRef
    return True

spTriggerOnce :: EntThink
spTriggerOnce = EntThink "SP_trigger_once" $ \edictRef -> do
    GameTrigger.spTriggerOnce edictRef
    return True

spTriggerMultiple :: EntThink
spTriggerMultiple = EntThink "SP_trigger_multiple" $ \edictRef -> do
    GameTrigger.spTriggerMultiple edictRef
    return True

spTriggerRelay :: EntThink
spTriggerRelay = EntThink "SP_trigger_relay" $ \edictRef -> do
    GameTrigger.spTriggerRelay edictRef
    return True

spTriggerPush :: EntThink
spTriggerPush = EntThink "SP_trigger_push" $ \edictRef -> do
    GameTrigger.spTriggerPush edictRef
    return True

spTriggerHurt :: EntThink
spTriggerHurt = EntThink "SP_trigger_hurt" $ \edictRef -> do
    GameTrigger.spTriggerHurt edictRef
    return True

spTriggerKey :: EntThink
spTriggerKey = EntThink "SP_trigger_key" $ \edictRef -> do
    GameTrigger.spTriggerKey edictRef
    return True

spTriggerCounter :: EntThink
spTriggerCounter = EntThink "SP_trigger_counter" $ \edictRef -> do
    GameTrigger.spTriggerCounter edictRef
    return True

spTriggerGravity :: EntThink
spTriggerGravity = EntThink "SP_trigger_gravity" $ \edictRef -> do
    GameTrigger.spTriggerGravity edictRef
    return True

spTriggerMonsterJump :: EntThink
spTriggerMonsterJump = EntThink "SP_trigger_monsterjump" $ \edictRef -> do
    GameTrigger.spTriggerMonsterJump edictRef
    return True

spTargetTempEntity :: EntThink
spTargetTempEntity = EntThink "SP_target_temp_entity" $ \edictRef -> do
    GameTarget.spTargetTempEntity edictRef
    return True

spTargetSpeaker :: EntThink
spTargetSpeaker = EntThink "SP_target_speaker" $ \edictRef -> do
    GameTarget.spTargetSpeaker edictRef
    return True

spTargetExplosion :: EntThink
spTargetExplosion = EntThink "SP_target_explosion" $ \edictRef -> do
    GameTarget.spTargetExplosion edictRef
    return True

spTargetChangeLevel :: EntThink
spTargetChangeLevel = EntThink "SP_target_changelevel" $ \edictRef -> do
    GameTarget.spTargetChangeLevel edictRef
    return True

spTargetSecret :: EntThink
spTargetSecret = EntThink "SP_target_secret" $ \edictRef -> do
    GameTarget.spTargetSecret edictRef
    return True

spTargetGoal :: EntThink
spTargetGoal = EntThink "SP_target_goal" $ \edictRef -> do
    GameTarget.spTargetGoal edictRef
    return True


spTargetSplash :: EntThink
spTargetSplash = EntThink "SP_target_splash" $ \edictRef -> do
    GameTarget.spTargetSplash edictRef
    return True

spTargetSpawner :: EntThink
spTargetSpawner = EntThink "SP_target_spawner" $ \edictRef -> do
    GameTarget.spTargetSpawner edictRef
    return True

spTargetBlaster :: EntThink
spTargetBlaster = EntThink "SP_target_blaster" $ \edictRef -> do
    GameTarget.spTargetBlaster edictRef
    return True

spTargetCrossLevelTrigger :: EntThink
spTargetCrossLevelTrigger = EntThink "SP_target_crosslevel_trigger" $ \edictRef -> do
    GameTarget.spTargetCrossLevelTrigger edictRef
    return True

spTargetCrossLevelTarget :: EntThink
spTargetCrossLevelTarget = EntThink "SP_target_crosslevel_target" $ \edictRef -> do
    GameTarget.spTargetCrossLevelTarget edictRef
    return True

spTargetLaser :: EntThink
spTargetLaser = EntThink "SP_target_laser" $ \edictRef -> do
    GameTarget.spTargetLaser edictRef
    return True

spTargetHelp :: EntThink
spTargetHelp = EntThink "SP_target_help" $ \edictRef -> do
    GameTarget.spTargetHelp edictRef
    return True

spTargetActor :: EntThink
spTargetActor = EntThink "SP_target_actor" $ \edictRef -> do
    MActor.spTargetActor edictRef
    return True

spTargetLightRamp :: EntThink
spTargetLightRamp = EntThink "SP_target_lightramp" $ \edictRef -> do
    GameTarget.spTargetLightRamp edictRef
    return True

spTargetEarthquake :: EntThink
spTargetEarthquake = EntThink "SP_target_earthquake" $ \edictRef -> do
    GameTarget.spTargetEarthquake edictRef
    return True

spTargetCharacter :: EntThink
spTargetCharacter = EntThink "SP_target_character" $ \edictRef -> do
    GameMisc.spTargetCharacter edictRef
    return True

spTargetString :: EntThink
spTargetString = EntThink "SP_target_string" $ \edictRef -> do
    GameMisc.spTargetString edictRef
    return True

spViewThing :: EntThink
spViewThing = EntThink "SP_viewthing" $ \edictRef -> do
    GameMisc.spViewThing edictRef
    return True

spLight :: EntThink
spLight = EntThink "SP_light" $ \edictRef -> do
    GameMisc.spLight edictRef
    return True

spLightMine1 :: EntThink
spLightMine1 = EntThink "SP_light_mine1" $ \edictRef -> do
    GameMisc.spLightMine1 edictRef
    return True

spLightMine2 :: EntThink
spLightMine2 = EntThink "SP_light_mine2" $ \edictRef -> do
    GameMisc.spLightMine2 edictRef
    return True

spInfoNull :: EntThink
spInfoNull = EntThink "SP_info_null" $ \edictRef -> do
    GameMisc.spInfoNull edictRef
    return True

spInfoNotNull :: EntThink
spInfoNotNull = EntThink "SP_info_notnull" $ \edictRef -> do
    GameMisc.spInfoNotNull edictRef
    return True

spPathCorner :: EntThink
spPathCorner = EntThink "SP_path_corner" $ \edictRef -> do
    GameMisc.spPathCorner edictRef
    return True

spPointCombat :: EntThink
spPointCombat = EntThink "SP_point_combat" $ \edictRef -> do
    GameMisc.spPointCombat edictRef
    return True

spMiscExploBox :: EntThink
spMiscExploBox = EntThink "SP_misc_explobox" $ \edictRef -> do
    GameMisc.spMiscExploBox edictRef
    return True

spMiscBanner :: EntThink
spMiscBanner = EntThink "SP_misc_banner" $ \edictRef -> do
    GameMisc.spMiscBanner edictRef
    return True

spMiscSatelliteDish :: EntThink
spMiscSatelliteDish = EntThink "SP_misc_satellite_dish" $ \edictRef -> do
    GameMisc.spMiscSatelliteDish edictRef
    return True

spMiscActor :: EntThink
spMiscActor = EntThink "SP_misc_actor" $ \edictRef -> do
    MActor.spMiscActor edictRef
    return False

spMiscGibArm :: EntThink
spMiscGibArm = EntThink "SP_misc_gib_arm" $ \edictRef -> do
    GameMisc.spMiscGibArm edictRef
    return True

spMiscGibLeg :: EntThink
spMiscGibLeg = EntThink "SP_misc_gib_leg" $ \edictRef -> do
    GameMisc.spMiscGibLeg edictRef
    return True

spMiscGibHead :: EntThink
spMiscGibHead = EntThink "SP_misc_gib_head" $ \edictRef -> do
    GameMisc.spMiscGibHead edictRef
    return True

spMiscInsane :: EntThink
spMiscInsane = EntThink "SP_misc_insane" $ \edictRef -> do
    MInsane.spMiscInsane edictRef
    return True

spMiscDeadSoldier :: EntThink
spMiscDeadSoldier = EntThink "SP_misc_deadsoldier" $ \edictRef -> do
    GameMisc.spMiscDeadSoldier edictRef
    return True

spMiscViper :: EntThink
spMiscViper = EntThink "SP_misc_viper" $ \edictRef -> do
    GameMisc.spMiscViper edictRef
    return True

spMiscViperBomb :: EntThink
spMiscViperBomb = EntThink "SP_misc_viper_bomb" $ \edictRef -> do
    GameMisc.spMiscViperBomb edictRef
    return True

spMiscBigViper :: EntThink
spMiscBigViper = EntThink "SP_misc_bigviper" $ \edictRef -> do
    GameMisc.spMiscBigViper edictRef
    return True

spMiscStroggShip :: EntThink
spMiscStroggShip = EntThink "SP_misc_strogg_ship" $ \edictRef -> do
    GameMisc.spMiscStroggShip edictRef
    return True

spMiscTeleporter :: EntThink
spMiscTeleporter = EntThink "SP_misc_teleporter" $ \edictRef -> do
    GameMisc.spMiscTeleporter edictRef
    return True

spMiscBlackHole :: EntThink
spMiscBlackHole = EntThink "SP_misc_blackhole" $ \edictRef -> do
    GameMisc.spMiscBlackHole edictRef
    return True

spMiscEasterTank :: EntThink
spMiscEasterTank = EntThink "SP_misc_eastertank" $ \edictRef -> do
    GameMisc.spMiscEasterTank edictRef
    return True

spMiscEasterChick :: EntThink
spMiscEasterChick = EntThink "SP_misc_easterchick" $ \edictRef -> do
    GameMisc.spMiscEasterChick edictRef
    return True

spMiscEasterChick2 :: EntThink
spMiscEasterChick2 = EntThink "SP_misc_easterchick2" $ \edictRef -> do
    GameMisc.spMiscEasterChick2 edictRef
    return True

spMonsterBerserk :: EntThink
spMonsterBerserk = EntThink "SP_monster_berserk" $ \edictRef -> do
    MBerserk.spMonsterBerserk edictRef
    return True

spMonsterGladiator :: EntThink
spMonsterGladiator = EntThink "SP_monster_gladiator" $ \edictRef -> do
    MGladiator.spMonsterGladiator edictRef
    return True

spMonsterGunner :: EntThink
spMonsterGunner = EntThink "SP_monster_gunner" $ \edictRef -> do
    MGunner.spMonsterGunner edictRef
    return True

spMonsterInfantry :: EntThink
spMonsterInfantry = EntThink "SP_monster_infantry" $ \edictRef -> do
    MInfantry.spMonsterInfantry edictRef
    return True

spMonsterMedic :: EntThink
spMonsterMedic = EntThink "SP_monster_medic" $ \edictRef -> do
    MMedic.spMonsterMedic edictRef
    return True

spMonsterFlipper :: EntThink
spMonsterFlipper = EntThink "SP_monster_flipper" $ \edictRef -> do
    MFlipper.spMonsterFlipper edictRef
    return True

spMonsterChick :: EntThink
spMonsterChick = EntThink "SP_monster_chick" $ \edictRef -> do
    MChick.spMonsterChick edictRef
    return True

spMonsterFlyer :: EntThink
spMonsterFlyer = EntThink "SP_monster_flyer" $ \edictRef -> do
    MFlyer.spMonsterFlyer edictRef
    return True

spMonsterBrain :: EntThink
spMonsterBrain = EntThink "SP_monster_brain" $ \edictRef -> do
    MBrain.spMonsterBrain edictRef
    return True

spMonsterFloater :: EntThink
spMonsterFloater = EntThink "SP_monster_floater" $ \edictRef -> do
    MFloat.spMonsterFloater edictRef
    return True

spMonsterHover :: EntThink
spMonsterHover = EntThink "SP_monster_hover" $ \edictRef -> do
    MHover.spMonsterHover edictRef
    return True

spMonsterBoss2 :: EntThink
spMonsterBoss2 = EntThink "SP_monster_boss2" $ \edictRef -> do
    MBoss2.spMonsterBoss2 edictRef
    return True

spMonsterBoss3Stand :: EntThink
spMonsterBoss3Stand = EntThink "SP_monster_boss3_stand" $ \edictRef -> do
    MBoss3.spMonsterBoss3Stand edictRef
    return True

spMonsterJorg :: EntThink
spMonsterJorg = EntThink "SP_monster_jorg" $ \edictRef -> do
    MBoss31.spMonsterJorg edictRef
    return True

spMonsterCommanderBody :: EntThink
spMonsterCommanderBody = EntThink "SP_monster_commander_body" $ \edictRef -> do
    GameMisc.spMonsterCommanderBody edictRef
    return True

spTurretBreach :: EntThink
spTurretBreach = EntThink "SP_turret_breach" $ \edictRef -> do
    GameTurret.spTurretBreach edictRef
    return True

spTurretBase :: EntThink
spTurretBase = EntThink "SP_turret_base" $ \edictRef -> do
    GameTurret.spTurretBase edictRef
    return True

spTurretDriver :: EntThink
spTurretDriver = EntThink "SP_turret_driver" $ \edictRef -> do
    GameTurret.spTurretDriver edictRef
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
