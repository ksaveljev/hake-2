module Game.GameSpawn
  ( callSpawn
  , spawnEntities
  ) where

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import qualified Game.GameFunc as GameFunc
import {-# SOURCE #-} Game.GameImportT
import qualified Game.GameItems as GameItems
import           Game.GameLocalsT 
import qualified Game.GameMisc as GameMisc
import qualified Game.GameTarget as GameTarget
import qualified Game.GameTrigger as GameTrigger
import qualified Game.GameTurret as GameTurret
import qualified Game.GameUtil as GameUtil
import           Game.GItemT
import           Game.LevelLocalsT
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
import           Game.SpawnT
import qualified QCommon.Com as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary (encode)

import           Control.Lens (use, (^.), (.=), (&), (.~), (%~))
import           Control.Monad (void, when)
import           Data.Bits (complement, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import           Data.Char (toLower)

spawnEntities :: B.ByteString -> B.ByteString -> B.ByteString -> Quake ()
spawnEntities mapName entities spawnPoint =
  do Com.dprintf (B.concat ["SpawnEntities(), mapname=", mapName, "\n"])
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
  when (skillValue /= skillLevel) $
    do forceSet <- use (gameBaseGlobals.gbGameImport.giCVarForceSet)
       void (forceSet "skill" (encode skillLevel))
       Com.dprintf (B.concat ["player skill level:", encode skillValue, "\n"])
  where skillValue = fromIntegral (floor (skill^.cvValue) :: Int) :: Float
        skillLevel | skillValue < 0 = 0
                   | skillValue > 3 = 3
                   | otherwise = skillValue

initEntities :: Quake ()
initEntities =
  do maxEntities <- use (gameBaseGlobals.gbGame.glMaxEntities)
     mapM_ (\idx -> writeRef (Ref Constants.noParent idx) (newEdictT idx)) [0..maxEntities-1]

initClients :: Quake ()
initClients =
  do maxClients <- use (gameBaseGlobals.gbGame.glMaxClients)
     mapM_ (\idx -> modifyRef (Ref Constants.noParent idx) (\v -> v & eClient .~ Just (Ref Constants.noParent (idx - 1)))) [1..maxClients]

parseAndSpawnEntities :: B.ByteString -> B.ByteString -> Quake ()
parseAndSpawnEntities entities mapName =
  do inhibited <- parseEntities entities mapName True 0 0
     Com.dprintf (encode inhibited `B.append` " entities inhibited.\n")

parseEntities :: B.ByteString -> B.ByteString -> Bool -> Int -> Int -> Quake Int
parseEntities entities mapName initial idx inhibited
  | idx >= B.length entities = return inhibited -- RESEARCH: make sure this is correct? is this present in jake2?
  | otherwise =
      do (token, updatedIdx) <- Com.parse entities (B.length entities) idx
         maybe (return inhibited) (processToken entities mapName initial updatedIdx inhibited) token

processToken :: B.ByteString -> B.ByteString -> Bool -> Int -> Int -> B.ByteString -> Quake Int
processToken entities mapName initial idx inhibited token =
  do checkError
     edictRef <- spawnEdict initial
     updatedIdx <- parseEdict edictRef entities idx
     printEdictInfo edictRef
     yetAnotherMapHack edictRef mapName
     removed <- removeThingsBasedOnSkillAndDeathmatch edictRef
     finishSpawn entities mapName updatedIdx inhibited edictRef removed
  where checkError = when (BC.head token /= '{') $
          do err <- use (gameBaseGlobals.gbGameImport.giError)
             err (B.concat ["ED_LoadFromFile: found ", token, " when expecting {"])

spawnEdict :: Bool -> Quake (Ref' EdictT)
spawnEdict initial
  | initial = return worldRef
  | otherwise = GameUtil.spawn

printEdictInfo :: Ref' EdictT -> Quake ()
printEdictInfo edictRef =
  do edict <- readRef edictRef
     Com.dprintf (B.concat [ "spawning ent[", encode (edict^.eIndex)
                           , "], classname=", edict^.eClassName
                           , ", flags=", encode (edict^.eSpawnFlags)
                           ])

yetAnotherMapHack :: Ref' EdictT -> B.ByteString -> Quake ()
yetAnotherMapHack edictRef mapName =
  do edict <- readRef edictRef
     when (shouldApplyHack edict (edict^.eiModel)) $
       modifyRef edictRef (\v -> v & eSpawnFlags %~ (.&. complement Constants.spawnFlagNotHard))
  where shouldApplyHack _ Nothing = False
        shouldApplyHack edict (Just model) =
          BC.map toLower mapName == "command" &&
          BC.map toLower (edict^.eClassName) == "trigger_once" &&
          BC.map toLower model == "*27"

removeThingsBasedOnSkillAndDeathmatch :: Ref' EdictT -> Quake Bool
removeThingsBasedOnSkillAndDeathmatch edictRef
  | edictRef == worldRef = return False
  | otherwise =
      do edict <- readRef edictRef
         freed <- checkDeathmatchAndSkill edictRef edict =<< deathmatchCVar
         checkFreed edictRef freed

checkDeathmatchAndSkill :: Ref' EdictT -> EdictT -> CVarT -> Quake Bool
checkDeathmatchAndSkill edictRef edict deathmatch
  | isDeathmatch && (edict^.eSpawnFlags) .&. Constants.spawnFlagNotDeathmatch /= 0 =
      inhibitEdict edictRef
  | isDeathmatch = return False
  | otherwise = checkSkill edictRef edict =<< skillCVar
  where isDeathmatch = (deathmatch^.cvValue) /= 0

checkSkill :: Ref' EdictT -> EdictT -> CVarT -> Quake Bool
checkSkill edictRef edict skill
  | wrongSkillLevel = inhibitEdict edictRef
  | otherwise = return False
  where wrongSkillLevel =
          ((skill^.cvValue) == 0 && (edict^.eSpawnFlags .&. Constants.spawnFlagNotEasy) /= 0) ||
          ((skill^.cvValue) == 1 && (edict^.eSpawnFlags .&. Constants.spawnFlagNotMedium) /= 0) ||
          (((skill^.cvValue) == 2 || (skill^.cvValue) == 3) && (edict^.eSpawnFlags .&. Constants.spawnFlagNotHard) /= 0)

inhibitEdict :: Ref' EdictT -> Quake Bool
inhibitEdict edictRef =
  do Com.dprintf "->inhibited.\n"
     GameUtil.freeEdict edictRef
     return True

checkFreed :: Ref' EdictT -> Bool -> Quake Bool
checkFreed edictRef freed
  | freed =
      do modifyRef edictRef (\v -> v & eSpawnFlags %~ (.&. complement flags))
         return True
  | otherwise = return False
  where flags = Constants.spawnFlagNotEasy .|. Constants.spawnFlagNotMedium .|.
                Constants.spawnFlagNotHard .|. Constants.spawnFlagNotCoop .|.
                Constants.spawnFlagNotDeathmatch

finishSpawn :: B.ByteString -> B.ByteString -> Int -> Int -> Ref' EdictT -> Bool -> Quake Int
finishSpawn entities mapName idx inhibited edictRef removed
  | removed = parseEntities entities mapName False idx (inhibited + 1)
  | otherwise =
      do callSpawn edictRef
         Com.dprintf "\n"
         parseEntities entities mapName False idx inhibited

findTeams :: Quake ()
findTeams =
  do numEdicts <- use (gameBaseGlobals.gbNumEdicts)
     (teamsNum, entitiesNum) <- findNextTeam 0 0 1 numEdicts
     dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
     dprintf (B.concat [encode teamsNum, " teams with ", encode entitiesNum, " entities\n"])

findNextTeam :: Int -> Int -> Int -> Int -> Quake (Int, Int)
findNextTeam c c2 idx maxIdx
  | idx >= maxIdx = return (c, c2)
  | otherwise =
      do edict <- readRef (Ref Constants.noParent idx)
         proceedFindNextTeam (Ref Constants.noParent idx) edict (edict^.eTeam)
  where proceedFindNextTeam _ _ Nothing = findNextTeam c c2 (idx + 1) maxIdx
        proceedFindNextTeam edictRef edict (Just team)
          | not (edict^.eInUse) || (edict^.eFlags) .&. Constants.flTeamSlave /= 0 =
              findNextTeam c c2 (idx + 1) maxIdx
          | otherwise =
              do modifyRef edictRef (\v ->v & eTeamMaster .~ Just edictRef)
                 c2' <- findTeamMembers team edictRef edictRef idx maxIdx c2
                 findNextTeam (c + 1) c2' (idx + 1) maxIdx

findTeamMembers :: B.ByteString -> Ref' EdictT -> Ref' EdictT -> Int -> Int -> Int -> Quake Int
findTeamMembers teamName master chainRef idx maxIdx c2
  | idx >= maxIdx = return c2
  | otherwise =
      do edict <- readRef (Ref Constants.noParent idx)
         proceedFindTeamMembers (Ref Constants.noParent idx) edict (edict^.eTeam)
  where proceedFindTeamMembers _ _ Nothing = findTeamMembers teamName master chainRef (idx + 1) maxIdx c2
        proceedFindTeamMembers edictRef edict (Just team)
          | not (edict^.eInUse) || (edict^.eFlags) .&. Constants.flTeamSlave /= 0 || teamName /= team =
              findTeamMembers teamName master chainRef (idx + 1) maxIdx c2
          | otherwise =
              do modifyRef chainRef (\v -> v & eTeamChain .~ Just edictRef)
                 modifyRef edictRef (\v -> v & eTeamMaster .~ Just master
                                             & eFlags %~ (.|. Constants.flTeamSlave))
                 findTeamMembers teamName master edictRef (idx + 1) maxIdx (c2 + 1)

parseEdict :: Ref' EdictT -> B.ByteString -> Int -> Quake Int
parseEdict = error "GameSpawn.parseEdict" -- TODO

callSpawn :: Ref' EdictT -> Quake ()
callSpawn edictRef =
  do edict <- readRef edictRef
     searchItems edictRef (BC.map toLower (edict^.eClassName))

searchItems :: Ref' EdictT -> B.ByteString -> Quake ()
searchItems edictRef className =
  do items <- use (gameBaseGlobals.gbItemList)
     case search (V.drop 1 items) of
       Just gItemRef -> GameItems.spawnItem edictRef gItemRef
       Nothing -> searchSpawns edictRef className
  where search items = fmap (Ref Constants.noParent) (V.findIndex searchByName items)
        searchByName gItem = className == BC.map toLower (gItem^.giClassName)

searchSpawns :: Ref' EdictT -> B.ByteString -> Quake ()
searchSpawns edictRef className =
  case spawnIdx of
    Just idx -> void (entThink ((spawns V.! idx)^.spSpawn) edictRef)
    Nothing -> do dprintf <- use (gameBaseGlobals.gbGameImport.giDprintf)
                  dprintf (className `B.append` " doesn't have a spawn function\n")
  where spawnIdx = V.findIndex searchByName spawns
        searchByName spawn = className == BC.map toLower (spawn^.spName)

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
spItemHealth = EntThink "GameSpawn.spItemHealth" $ \edictRef ->
  do GameItems.spItemHealth edictRef
     return True

spItemHealthSmall :: EntThink
spItemHealthSmall = EntThink "GameSpawn.spItemHealthSmall" $ \edictRef ->
  do GameItems.spItemHealthSmall edictRef
     return True

spItemHealthLarge :: EntThink
spItemHealthLarge = EntThink "GameSpawn.spItemHealthLarge" $ \edictRef ->
  do GameItems.spItemHealthLarge edictRef
     return True

spItemHealthMega :: EntThink
spItemHealthMega = EntThink "GameSpawn.spItemHealthMega" $ \edictRef ->
  do GameItems.spItemHealthMega edictRef
     return True

spInfoPlayerStart :: EntThink
spInfoPlayerStart = EntThink "GameSpawn.spInfoPlayerStart" $ \edictRef ->
  do PlayerClient.spInfoPlayerStart edictRef
     return True

spInfoPlayerDeathmatch :: EntThink
spInfoPlayerDeathmatch = EntThink "GameSpawn.spInfoPlayerDeathmatch" $ \edictRef ->
  do PlayerClient.spInfoPlayerDeathmatch edictRef
     return True

spInfoPlayerCoop :: EntThink
spInfoPlayerCoop = EntThink "GameSpawn.spInfoPlayerCoop" $ \edictRef ->
  do PlayerClient.spInfoPlayerCoop edictRef
     return True

spInfoPlayerIntermission :: EntThink
spInfoPlayerIntermission = EntThink "GameSpawn.spInfoPlayerIntermission" $ \_ ->
  do PlayerClient.spInfoPlayerIntermission
     return True

spFuncPlat :: EntThink
spFuncPlat = EntThink "GameSpawn.spFuncPlat" $ \edictRef ->
  do GameFunc.spFuncPlat edictRef
     return True

spFuncWater :: EntThink
spFuncWater = EntThink "GameSpawn.spFuncWater" $ \edictRef ->
  do GameFunc.spFuncWater edictRef
     return True

spFuncTrain :: EntThink
spFuncTrain = EntThink "GameSpawn.spFuncTrain" $ \edictRef ->
  do GameFunc.spFuncTrain edictRef
     return True

spFuncClock :: EntThink
spFuncClock = EntThink "GameSpawn.spFuncClock" $ \edictRef ->
  do GameMisc.spFuncClock edictRef
     return True

spWorldSpawn :: EntThink
spWorldSpawn = EntThink "GameSpawn.spWorldSpawn" (\_ -> error "GameSpawn.spWorldSpawn") -- TODO

spFuncWall :: EntThink
spFuncWall = EntThink "GameSpawn.spFuncWall" $ \edictRef ->
  do GameMisc.spFuncWall edictRef
     return True

spFuncObject :: EntThink
spFuncObject = EntThink "GameSpawn.spFuncObject" $ \edictRef ->
  do GameMisc.spFuncObject edictRef
     return True

spFuncTimer :: EntThink
spFuncTimer = EntThink "GameSpawn.spFuncTimer" $ \edictRef ->
  do GameFunc.spFuncTimer edictRef
     return True

spFuncExplosive :: EntThink
spFuncExplosive = EntThink "GameSpawn.spFuncExplosive" $ \edictRef ->
  do GameMisc.spFuncExplosive edictRef
     return True

spTriggerAlways :: EntThink
spTriggerAlways = EntThink "GameSpawn.spTriggerAlways" $ \edictRef ->
  do GameTrigger.spTriggerAlways edictRef
     return True

spTriggerOnce :: EntThink
spTriggerOnce = EntThink "GameSpawn.spTriggerOnce" $ \edictRef ->
  do GameTrigger.spTriggerOnce edictRef
     return True

spTriggerMultiple :: EntThink
spTriggerMultiple = EntThink "GameSpawn.spTriggerMultiple" $ \edictRef ->
  do GameTrigger.spTriggerMultiple edictRef
     return True

spTriggerRelay :: EntThink
spTriggerRelay = EntThink "GameSpawn.spTriggerRelay" $ \edictRef ->
  do GameTrigger.spTriggerRelay edictRef
     return True

spTriggerPush :: EntThink
spTriggerPush = EntThink "GameSpawn.spTriggerPush" $ \edictRef ->
  do GameTrigger.spTriggerPush edictRef
     return True

spTriggerHurt :: EntThink
spTriggerHurt = EntThink "GameSpawn.spTriggerHurt" $ \edictRef ->
  do GameTrigger.spTriggerHurt edictRef
     return True

spTriggerKey :: EntThink
spTriggerKey = EntThink "GameSpawn.spTriggerKey" $ \edictRef ->
  do GameTrigger.spTriggerKey edictRef
     return True

spTriggerCounter :: EntThink
spTriggerCounter = EntThink "GameSpawn.spTriggerCounter" $ \edictRef ->
  do GameTrigger.spTriggerCounter edictRef
     return True

spTriggerGravity :: EntThink
spTriggerGravity = EntThink "GameSpawn.spTriggerGravity" $ \edictRef ->
  do GameTrigger.spTriggerGravity edictRef
     return True

spTriggerMonsterJump :: EntThink
spTriggerMonsterJump = EntThink "GameSpawn.spTriggerMonsterJump" $ \edictRef ->
  do GameTrigger.spTriggerMonsterJump edictRef
     return True

spTargetTempEntity :: EntThink
spTargetTempEntity = EntThink "GameSpawn.spTargetTempEntity" $ \edictRef ->
  do GameTarget.spTargetTempEntity edictRef
     return True

spTargetSpeaker :: EntThink
spTargetSpeaker = EntThink "GameSpawn.spTargetSpeaker" $ \edictRef ->
  do GameTarget.spTargetSpeaker edictRef
     return True

spTargetExplosion :: EntThink
spTargetExplosion = EntThink "GameSpawn.spTargetExplosion" $ \edictRef ->
  do GameTarget.spTargetExplosion edictRef
     return True

spTargetChangeLevel :: EntThink
spTargetChangeLevel = EntThink "GameSpawn.spTargetChangeLevel" $ \edictRef ->
  do GameTarget.spTargetChangeLevel edictRef
     return True

spTargetSecret :: EntThink
spTargetSecret = EntThink "GameSpawn.spTargetSecret" $ \edictRef ->
  do GameTarget.spTargetSecret edictRef
     return True

spTargetGoal :: EntThink
spTargetGoal = EntThink "GameSpawn.spTargetGoal" $ \edictRef ->
  do GameTarget.spTargetGoal edictRef
     return True

spTargetSplash :: EntThink
spTargetSplash = EntThink "GameSpawn.spTargetSplash" $ \edictRef ->
  do GameTarget.spTargetSplash edictRef
     return True

spTargetSpawner :: EntThink
spTargetSpawner = EntThink "GameSpawn.spTargetSpawner" $ \edictRef ->
  do GameTarget.spTargetSpawner edictRef
     return True

spTargetBlaster :: EntThink
spTargetBlaster = EntThink "GameSpawn.spTargetBlaster" $ \edictRef ->
  do GameTarget.spTargetBlaster edictRef
     return True

spTargetCrossLevelTrigger :: EntThink
spTargetCrossLevelTrigger = EntThink "GameSpawn.spTargetCrossLevelTrigger" $ \edictRef ->
  do GameTarget.spTargetCrossLevelTrigger edictRef
     return True

spTargetCrossLevelTarget :: EntThink
spTargetCrossLevelTarget = EntThink "GameSpawn.spTargetCrossLevelTarget" $ \edictRef ->
  do GameTarget.spTargetCrossLevelTarget edictRef
     return True

spTargetLaser :: EntThink
spTargetLaser = EntThink "GameSpawn.spTargetLaser" $ \edictRef ->
  do GameTarget.spTargetLaser edictRef
     return True

spTargetHelp :: EntThink
spTargetHelp = EntThink "GameSpawn.spTargetHelp" $ \edictRef ->
  do GameTarget.spTargetHelp edictRef
     return True

spTargetActor :: EntThink
spTargetActor = EntThink "GameSpawn.spTargetActor" $ \edictRef ->
  do MActor.spTargetActor edictRef
     return True

spTargetLightRamp :: EntThink
spTargetLightRamp = EntThink "GameSpawn.spTargetLightRamp" $ \edictRef ->
  do GameTarget.spTargetLightRamp edictRef
     return True

spTargetEarthquake :: EntThink
spTargetEarthquake = EntThink "GameSpawn.spTargetEarthquake" $ \edictRef ->
  do GameTarget.spTargetEarthquake edictRef
     return True

spTargetCharacter :: EntThink
spTargetCharacter = EntThink "GameSpawn.spTargetCharacter" $ \edictRef ->
  do GameMisc.spTargetCharacter edictRef
     return True

spTargetString :: EntThink
spTargetString = EntThink "GameSpawn.spTargetString" $ \edictRef ->
  do GameMisc.spTargetString edictRef
     return True

spViewThing :: EntThink
spViewThing = EntThink "GameSpawn.spViewThing" $ \edictRef ->
  do GameMisc.spViewThing edictRef
     return True

spLight :: EntThink
spLight = EntThink "GameSpawn.spLight" $ \edictRef ->
  do GameMisc.spLight edictRef
     return True

spLightMine1 :: EntThink
spLightMine1 = EntThink "GameSpawn.spLightMine1" $ \edictRef ->
  do GameMisc.spLightMine1 edictRef
     return True

spLightMine2 :: EntThink
spLightMine2 = EntThink "GameSpawn.spLightMine2" $ \edictRef ->
  do GameMisc.spLightMine2 edictRef
     return True

spInfoNull :: EntThink
spInfoNull = EntThink "GameSpawn.spInfoNull" $ \edictRef ->
  do GameMisc.spInfoNull edictRef
     return True

spInfoNotNull :: EntThink
spInfoNotNull = EntThink "GameSpawn.spInfoNotNull" $ \edictRef ->
  do GameMisc.spInfoNotNull edictRef
     return True

spPathCorner :: EntThink
spPathCorner = EntThink "GameSpawn.spPathCorner" $ \edictRef ->
  do GameMisc.spPathCorner edictRef
     return True

spPointCombat :: EntThink
spPointCombat = EntThink "GameSpawn.spPointCombat" $ \edictRef ->
  do GameMisc.spPointCombat edictRef
     return True

spMiscExploBox :: EntThink
spMiscExploBox = EntThink "GameSpawn.spMiscExploBox" $ \edictRef ->
  do GameMisc.spMiscExploBox edictRef
     return True

spMiscBanner :: EntThink
spMiscBanner = EntThink "GameSpawn.spMiscBanner" $ \edictRef ->
  do GameMisc.spMiscBanner edictRef
     return True

spMiscSatelliteDish :: EntThink
spMiscSatelliteDish = EntThink "GameSpawn.spMiscSatelliteDish" $ \edictRef ->
  do GameMisc.spMiscSatelliteDish edictRef
     return True

spMiscActor :: EntThink
spMiscActor = EntThink "GameSpawn.spMiscActor" $ \edictRef ->
  do MActor.spMiscActor edictRef
     return False

spMiscGibArm :: EntThink
spMiscGibArm = EntThink "GameSpawn.spMiscGibArm" $ \edictRef ->
  do GameMisc.spMiscGibArm edictRef
     return True

spMiscGibLeg :: EntThink
spMiscGibLeg = EntThink "GameSpawn.spMiscGibLeg" $ \edictRef ->
  do GameMisc.spMiscGibLeg edictRef
     return True

spMiscGibHead :: EntThink
spMiscGibHead = EntThink "GameSpawn.spMiscGibHead" $ \edictRef ->
  do GameMisc.spMiscGibHead edictRef
     return True

spMiscInsane :: EntThink
spMiscInsane = EntThink "GameSpawn.spMiscInsane" $ \edictRef ->
  do MInsane.spMiscInsane edictRef
     return True

spMiscDeadSoldier :: EntThink
spMiscDeadSoldier = EntThink "GameSpawn.spMiscDeadSoldier" $ \edictRef ->
  do GameMisc.spMiscDeadSoldier edictRef
     return True

spMiscViper :: EntThink
spMiscViper = EntThink "GameSpawn.spMiscViper" $ \edictRef ->
  do GameMisc.spMiscViper edictRef
     return True

spMiscViperBomb :: EntThink
spMiscViperBomb = EntThink "GameSpawn.spMiscViperBomb" $ \edictRef ->
  do GameMisc.spMiscViperBomb edictRef
     return True

spMiscBigViper :: EntThink
spMiscBigViper = EntThink "GameSpawn.spMiscBigViper" $ \edictRef ->
  do GameMisc.spMiscBigViper edictRef
     return True

spMiscStroggShip :: EntThink
spMiscStroggShip = EntThink "GameSpawn.spMiscStroggShip" $ \edictRef ->
  do GameMisc.spMiscStroggShip edictRef
     return True

spMiscTeleporter :: EntThink
spMiscTeleporter = EntThink "GameSpawn.spMiscTeleporter" $ \edictRef ->
  do GameMisc.spMiscTeleporter edictRef
     return True

spMiscBlackHole :: EntThink
spMiscBlackHole = EntThink "GameSpawn.spMiscBlackHole" $ \edictRef ->
  do GameMisc.spMiscBlackHole edictRef
     return True

spMiscEasterTank :: EntThink
spMiscEasterTank = EntThink "GameSpawn.spMiscEasterTank" $ \edictRef ->
  do GameMisc.spMiscEasterTank edictRef
     return True

spMiscEasterChick :: EntThink
spMiscEasterChick = EntThink "GameSpawn.spMiscEasterChick" $ \edictRef ->
  do GameMisc.spMiscEasterChick edictRef
     return True

spMiscEasterChick2 :: EntThink
spMiscEasterChick2 = EntThink "GameSpawn.spMiscEasterChick2" $ \edictRef ->
  do GameMisc.spMiscEasterChick2 edictRef
     return True

spMonsterBerserk :: EntThink
spMonsterBerserk = EntThink "GameSpawn.spMonsterBerserk" $ \edictRef ->
  do MBerserk.spMonsterBerserk edictRef
     return True

spMonsterGladiator :: EntThink
spMonsterGladiator = EntThink "GameSpawn.spMonsterGladiator" $ \edictRef ->
  do MGladiator.spMonsterGladiator edictRef
     return True

spMonsterGunner :: EntThink
spMonsterGunner = EntThink "GameSpawn.spMonsterGunner" $ \edictRef ->
  do MGunner.spMonsterGunner edictRef
     return True

spMonsterInfantry :: EntThink
spMonsterInfantry = EntThink "GameSpawn.spMonsterInfantry" $ \edictRef ->
  do MInfantry.spMonsterInfantry edictRef
     return True

spMonsterMedic :: EntThink
spMonsterMedic = EntThink "GameSpawn.spMonsterMedic" $ \edictRef ->
  do MMedic.spMonsterMedic edictRef
     return True

spMonsterFlipper :: EntThink
spMonsterFlipper = EntThink "GameSpawn.spMonsterFlipper" $ \edictRef ->
  do MFlipper.spMonsterFlipper edictRef
     return True

spMonsterChick :: EntThink
spMonsterChick = EntThink "GameSpawn.spMonsterChick" $ \edictRef ->
  do MChick.spMonsterChick edictRef
     return True

spMonsterFlyer :: EntThink
spMonsterFlyer = EntThink "GameSpawn.spMonsterFlyer" $ \edictRef ->
  do MFlyer.spMonsterFlyer edictRef
     return True

spMonsterBrain :: EntThink
spMonsterBrain = EntThink "GameSpawn.spMonsterBrain" $ \edictRef ->
  do MBrain.spMonsterBrain edictRef
     return True

spMonsterFloater :: EntThink
spMonsterFloater = EntThink "GameSpawn.spMonsterFloater" $ \edictRef ->
  do MFloat.spMonsterFloater edictRef
     return True

spMonsterHover :: EntThink
spMonsterHover = EntThink "GameSpawn.spMonsterHover" $ \edictRef ->
  do MHover.spMonsterHover edictRef
     return True

spMonsterBoss2 :: EntThink
spMonsterBoss2 = EntThink "GameSpawn.spMonsterBoss2" $ \edictRef ->
  do MBoss2.spMonsterBoss2 edictRef
     return True

spMonsterBoss3Stand :: EntThink
spMonsterBoss3Stand = EntThink "GameSpawn.spMonsterBoss3Stand" $ \edictRef ->
  do MBoss3.spMonsterBoss3Stand edictRef
     return True

spMonsterJorg :: EntThink
spMonsterJorg = EntThink "GameSpawn.spMonsterJorg" $ \edictRef ->
  do MBoss31.spMonsterJorg edictRef
     return True

spMonsterCommanderBody :: EntThink
spMonsterCommanderBody = EntThink "GameSpawn.spMonsterCommanderBody" $ \edictRef ->
  do GameMisc.spMonsterCommanderBody edictRef
     return True

spTurretBreach :: EntThink
spTurretBreach = EntThink "GameSpawn.spTurretBreach" $ \edictRef ->
  do GameTurret.spTurretBreach edictRef
     return True

spTurretBase :: EntThink
spTurretBase = EntThink "GameSpawn.spTurretBase" $ \edictRef ->
  do GameTurret.spTurretBase edictRef
     return True

spTurretDriver :: EntThink
spTurretDriver = EntThink "GameSpawn.spTurretDriver" $ \edictRef ->
 do GameTurret.spTurretDriver edictRef
    return True