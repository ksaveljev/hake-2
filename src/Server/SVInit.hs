{-# LANGUAGE FlexibleContexts #-}
module Server.SVInit
  ( imageIndex
  , modelIndex
  , soundIndex
  , svMap
  ) where

import qualified Client.CL as CL
import qualified Client.SCR as SCR
import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameBase as GameBase
import qualified Game.GameSpawn as GameSpawn
import           Game.UserCmdT
import qualified QCommon.CBuf as CBuf
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import qualified QCommon.SZ as SZ
import           QuakeRef
import           QuakeState
import           Server.ClientT
import           Server.ServerStaticT
import           Server.ServerT
import qualified Server.SVGame as SVGame
import qualified Server.SVMainShared as SVMain
import qualified Server.SVSend as SVSend
import qualified Server.SVWorld as SVWorld
import qualified Sys.NET as NET
import           Types
import           Util.Binary (encode)
import qualified Util.Lib as Lib

import           Control.Lens (use, zoom, ix, (^.), (.=), (+=), (%=), (&), (.~), (%~))
import           Control.Monad (void, when, (>=>))
import           Control.Monad.Coroutine (mapMonad)
import           Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

svMap :: Bool -> B.ByteString -> Bool -> Quake ()
svMap attractLoop levelString loadGame =
  do mapMonad (zoom (svGlobals.svServer)) $ do
       sLoadGame .= loadGame
       sAttractLoop .= attractLoop
     checkInitGame loadGame =<< use (svGlobals.svServer.sState)
     level <- extractLevelAndNextServer levelString
     checkFirstMap levelString =<< use (svGlobals.svFirstMap)
     coopEndGameScreenHack level =<< CVar.variableValue "coop"
     spawnLevelAndReconnect level attractLoop loadGame

checkInitGame :: Bool -> Int -> Quake ()
checkInitGame loadGame state
  | state == Constants.ssDead && not loadGame = initGame
  | otherwise = return ()

extractLevelAndNextServer :: B.ByteString -> Quake B.ByteString
extractLevelAndNextServer levelString = maybe noNextServer setNextServer splitIdx
  where splitIdx = '+' `BC.elemIndex` levelString
        noNextServer =
          do void (CVar.set "nextserver" B.empty)
             return levelString
        setNextServer idx =
          do void (CVar.set "nextserver" (B.concat ["gamemap \"", B.drop (idx + 1) levelString, "\""]))
             return (B.take idx levelString)

checkFirstMap :: B.ByteString -> B.ByteString -> Quake ()
checkFirstMap levelString firstMap
  | isEmptyMap && not isResourceFile = svGlobals.svFirstMap .= fm ('+' `BC.elemIndex` levelString)
  | otherwise = return ()
  where isEmptyMap = B.null firstMap
        isResourceFile = or (fmap (`BC.isSuffixOf` levelString) [".cin", ".pcx", ".dm2"])
        fm Nothing = levelString
        fm (Just idx) = B.drop (idx + 1) levelString

coopEndGameScreenHack :: B.ByteString -> Float -> Quake ()
coopEndGameScreenHack level coop
  | coop /= 0 && level == "victory.pcx" =
      do fm <- use (svGlobals.svFirstMap)
         void (CVar.set "nextserver" ("gamemap \"*" `B.append` fm `B.append` "\""))
  | otherwise = return ()

spawnLevelAndReconnect :: B.ByteString -> Bool -> Bool -> Quake ()
spawnLevelAndReconnect level attractLoop loadGame =
  do SCR.beginLoadingPlaque
     SVSend.broadcastCommand "changing\n"
     spawnServerBasedOnType finalLevel spawnPoint attractLoop loadGame
     SVSend.broadcastCommand "reconnect\n"
  where (spawnPoint, updatedLevel) = checkSpawnPoint ('$' `BC.elemIndex` level)
        checkSpawnPoint Nothing = (B.empty, level)
        checkSpawnPoint (Just idx) = (B.drop (idx + 1) level, B.take idx level)
        finalLevel | BC.head updatedLevel == '*' = B.drop 1 updatedLevel -- TODO: what if empty updatedLevel?
                   | otherwise = updatedLevel

spawnServerBasedOnType :: B.ByteString -> B.ByteString -> Bool -> Bool -> Quake ()
spawnServerBasedOnType finalLevel spawnPoint attractLoop loadGame
  | len > 4 && ".cin" `BC.isSuffixOf` finalLevel =
      spawnServer finalLevel spawnPoint Constants.ssCinematic attractLoop loadGame
  | len > 4 && ".dm2" `BC.isSuffixOf` finalLevel =
      spawnServer finalLevel spawnPoint Constants.ssDemo attractLoop loadGame
  | len > 4 && ".pcx" `BC.isSuffixOf` finalLevel =
      spawnServer finalLevel spawnPoint Constants.ssPic attractLoop loadGame
  | otherwise =
      do SVSend.sendClientMessages
         spawnServer finalLevel spawnPoint Constants.ssGame attractLoop loadGame
         CBuf.copyToDefer
  where len = B.length finalLevel

findIndex :: Maybe B.ByteString -> Int -> Int -> Bool -> Quake Int
findIndex = error "SVInit.findIndex" -- TODO

modelIndex :: Maybe B.ByteString -> Quake Int
modelIndex name = findIndex name Constants.csModels Constants.maxModels True

soundIndex :: Maybe B.ByteString -> Quake Int
soundIndex name = findIndex name Constants.csSounds Constants.maxSounds True

imageIndex :: Maybe B.ByteString -> Quake Int
imageIndex name = findIndex name Constants.csImages Constants.maxImages True

initGame :: Quake ()
initGame =
  do restartServer =<< use (svGlobals.svServerStatic.ssInitialized)
     CVar.getLatchedVars
     svGlobals.svServerStatic.ssInitialized .= True
     checkCoopDeathmatch
     checkCoopDedicated
     initClients
     setServerStaticSettings
     NET.config . (> 1) =<< fmap (^.cvValue) maxClientsCVar
     initHeartbeat
     SVGame.initGameProgs
     resetClients

restartServer :: Bool -> Quake ()
restartServer True = SVMain.shutdown "Server restarted\n" True
restartServer False =
  do CL.dropClient
     SCR.beginLoadingPlaque

checkCoopDeathmatch :: Quake ()
checkCoopDeathmatch =
  do coop <- CVar.variableValue "coop"
     deathmatch <- CVar.variableValue "deathmatch"
     when (coop /= 0 && deathmatch /= 0) $
       do Com.printf "Deathmatch and Coop both set, disabling Coop\n"
          void (CVar.fullSet "coop" "0" (Constants.cvarServerInfo .|. Constants.cvarLatch))

checkCoopDedicated :: Quake ()
checkCoopDedicated =
  do coop <- CVar.variableValue "coop"
     dedicated <- dedicatedCVar
     when ((dedicated^.cvValue) /= 0 && coop == 0) $
       void (CVar.fullSet "deathmatch" "1" (Constants.cvarServerInfo .|. Constants.cvarLatch))

initClients :: Quake ()
initClients =
  do coop <- CVar.variableValue "coop"
     deathmatch <- CVar.variableValue "deathmatch"
     maxClients <- fmap (truncate .(^.cvValue)) maxClientsCVar
     doInitClients coop deathmatch maxClients

doInitClients :: Float -> Float -> Int -> Quake ()
doInitClients coop deathmatch maxClients
  | deathmatch /= 0 = setDeathmatchMaxClients
  | coop /= 0 = setCoopMaxClients
  | otherwise = void (CVar.fullSet "maxclients" "1" serverLatchFlags)
  where setDeathmatchMaxClients
          | maxClients <= 1 = void $ CVar.fullSet "maxclients" "8" serverLatchFlags
          | maxClients > Constants.maxClients = void (CVar.fullSet "maxclients" (encode Constants.maxClients) serverLatchFlags)
          | otherwise = return ()
        setCoopMaxClients
          | maxClients <= 1 || maxClients > 4 = void (CVar.fullSet "maxclients" "4" serverLatchFlags)
          | otherwise = return ()
        serverLatchFlags = Constants.cvarServerInfo .|. Constants.cvarLatch

setServerStaticSettings :: Quake ()
setServerStaticSettings =
  do r <- Lib.rand
     maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
     mapMonad (zoom (svGlobals.svServerStatic)) $
       do ssSpawnCount .= fromIntegral r
          ssClients .= V.generate maxClients (\idx -> newClientT & cServerIndex .~ idx)
          ssNumClientEntities .= maxClients * Constants.updateBackup * 64
          ssClientEntities .= V.replicate (maxClients * Constants.updateBackup * 64) (newEntityStateT Nothing)

initHeartbeat :: Quake ()
initHeartbeat =
  do svGlobals.svServerStatic.ssLastHeartbeat .= -99999
     idMasterNetAdr <- NET.stringToAdr ("192.246.40.37:" `B.append` encode Constants.portMaster)
     maybe netAdrError setMasterAdr idMasterNetAdr
  where netAdrError = error "SVInit.initHeartbeat idMasterNetAdr is Nothing"
        setMasterAdr adr = svGlobals.svMasterAdr %= (V.// [(0, adr)])

resetClients :: Quake ()
resetClients =
  svGlobals.svServerStatic.ssClients %= V.imap resetClient
  where resetClient idx client = client & cEdict .~ Just (Ref (idx + 1))
                                        & cLastCmd .~ newUserCmdT

spawnServer :: B.ByteString -> B.ByteString -> Int -> Bool -> Bool -> Quake ()
spawnServer server spawnPoint srvState attractLoop loadGame =
  do when attractLoop (void (CVar.set "paused" "0"))
     Com.printf "------- Server Initialization -------\n"
     Com.dprintf (B.concat ["SpawnServer: ", server, "\n"])
     maybe (return ()) Lib.fClose =<< use (svGlobals.svServer.sDemoFile)
     spawnServerUpdateState server attractLoop loadGame
     spawnServerDeathmatchBasedSettings =<< CVar.variableValue "deathmatch"
     initServerMulticast
     initServerClientsAndModels server srvState
     SVWorld.clearWorld
     updateServerModels
     spawnMapEntities server spawnPoint srvState

spawnServerUpdateState :: B.ByteString -> Bool -> Bool -> Quake ()
spawnServerUpdateState server attractLoop loadGame =
  do svGlobals.svServerStatic.ssSpawnCount += 1
     svGlobals.svServer.sState .= Constants.ssDead
     globals.gServerState .= Constants.ssDead
     svGlobals.svServer .= (newServerT & sLoadGame .~ loadGame
                                       & sAttractLoop .~ attractLoop
                                       & sConfigStrings %~ (V.// [(Constants.csName, server)]))
     svGlobals.svServerStatic.ssRealTime .= 0

spawnServerDeathmatchBasedSettings :: Float -> Quake ()
spawnServerDeathmatchBasedSettings deathmatch
  | deathmatch /= 0 =
      do airAccelerate <- fmap (^.cvValue) svAirAccelerateCVar
         svGlobals.svServer.sConfigStrings %= (V.// [(Constants.csAirAccel, encode airAccelerate)]) -- IMPROVE: convert Float to ByteString using binary package?
         pMoveGlobals.pmAirAccelerate .= airAccelerate
  | otherwise =
      do svGlobals.svServer.sConfigStrings %= (V.// [(Constants.csAirAccel, "0")])
         pMoveGlobals.pmAirAccelerate .= 0

initServerMulticast :: Quake ()
initServerMulticast =
  do bufData <- use (svGlobals.svServer.sMulticastBuf)
     SZ.initialize (svGlobals.svServer.sMulticast) bufData Constants.maxMsgLen

initServerClientsAndModels :: B.ByteString -> Int -> Quake ()
initServerClientsAndModels server srvState =
  do mapMonad (zoom (svGlobals.svServer)) $ do
       sName .= server
       sTime .= 1000
       sConfigStrings %= (V.// [(Constants.csName, server)])
     svGlobals.svServerStatic.ssClients %= fmap updateClientState
     (modelRef, iw) <- loadContent
     svGlobals.svServer.sModels %= (V.// [(1, modelRef)])
     svGlobals.svServer.sConfigStrings %= (V.// [(Constants.csMapChecksum, encode (head iw))])
  where loadContent | srvState /= Constants.ssGame = CM.loadMap B.empty False [0] -- no real map
                    | otherwise =
                        do svGlobals.svServer.sConfigStrings %= (V.// [(Constants.csModels + 1, mapName)])
                           CM.loadMap mapName False [0]
        mapName = B.concat ["maps/", server, ".bsp"]

updateClientState :: ClientT -> ClientT
updateClientState client = client & cState .~ updatedState
                                  & cLastFrame .~ (-1)
  where updatedState
          | (client^.cState) > Constants.csConnected = Constants.csConnected
          | otherwise = client^.cState

updateServerModels :: Quake ()
updateServerModels =
  do modelsCount <- CM.numInlineModels
     svGlobals.svServer.sConfigStrings %= (V.// fmap updateModelsConfigStrings [1..modelsCount-1])
     updatedModels <- mapM inlineModels [1..modelsCount-1]
     svGlobals.svServer.sModels %= (V.// updatedModels)
  where updateModelsConfigStrings idx = (Constants.csModels + 1 + idx, "*" `B.append` encode idx) 
        inlineModels idx = CM.inlineModel ("*" `B.append` encode idx) >>= \ref -> return (idx + 1, ref)

spawnMapEntities :: B.ByteString -> B.ByteString -> Int -> Quake ()
spawnMapEntities server spawnPoint srvState =
  do svGlobals.svServer.sState .= Constants.ssLoading
     globals.gServerState .= Constants.ssLoading
     es <- CM.entityString
     GameSpawn.spawnEntities server es spawnPoint
     GameBase.runFrame -- run two frames to allow everything to settle
     GameBase.runFrame
     svGlobals.svServer.sState .= srvState
     globals.gServerState .= srvState
     createBaseline
     checkForSavegame
     void (CVar.fullSet "mapname" server (Constants.cvarServerInfo .|. Constants.cvarNoSet))

createBaseline :: Quake ()
createBaseline =
  do numEdicts <- use (gameBaseGlobals.gbNumEdicts)
     mapM_ (readEdict >=> edictBaseline) [1..numEdicts-1]

readEdict :: Int -> Quake (Ref' EdictT, EdictT)
readEdict idx =
  do edict <- readRef (Ref idx)
     return (Ref idx, edict)

edictBaseline :: (Ref' EdictT, EdictT) -> Quake ()
edictBaseline (edictRef@(Ref idx), edict)
  | not (edict^.eInUse) || ((edict^.eEntityState.esModelIndex) == 0 && (edict^.eEntityState.esSound) == 0 && (edict^.eEntityState.esEffects) == 0) =
      return ()
  | otherwise =
      do modifyRef edictRef (\v -> v & eEntityState.esNumber .~ idx
                                     & eEntityState.esOldOrigin .~ (edict^.eEntityState.esOrigin))
         saveEdictEntityState edictRef

saveEdictEntityState :: Ref' EdictT -> Quake ()
saveEdictEntityState edictRef@(Ref idx) =
  do edict <- readRef edictRef
     svGlobals.svServer.sBaselines.ix idx .= (edict^.eEntityState)

checkForSavegame :: Quake ()
checkForSavegame = request (io (putStrLn "SVInit.checkForSavegame IMPLEMENT ME!"))
