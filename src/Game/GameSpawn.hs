module Game.GameSpawn
    ( callSpawn
    , spawnEntities
    ) where

import           Control.Lens          (use, (^.), (.=), (&), (.~), (%~))
import           Control.Monad         (void, when)
import           Data.Bits             (complement, (.&.), (.|.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector           as V
import           Data.Char             (toLower)

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import qualified Game.GameFunc         as GameFunc
import {-# SOURCE #-} Game.GameImportT
import qualified Game.GameItems        as GameItems
import           Game.GameLocalsT 
import qualified Game.GameMisc         as GameMisc
import qualified Game.GameTarget       as GameTarget
import qualified Game.GameTrigger      as GameTrigger
import qualified Game.GameTurret       as GameTurret
import qualified Game.GameUtil         as GameUtil
import           Game.GItemT
import           Game.LevelLocalsT
import qualified Game.PlayerClient     as PlayerClient
import qualified Game.PlayerTrail      as PlayerTrail
import           Game.SpawnT
import qualified QCommon.Com           as Com
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import           Types
import           Util.Binary           (encode)

callSpawn :: Ref EdictT -> Quake ()
callSpawn = error "GameSpawn.callSpawn" -- TODO

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
parseEdict = error "GameSpawn.parseEdict" -- TODO