module Server.SVInit
  ( imageIndex
  , modelIndex
  , soundIndex
  , svMap
  ) where

import qualified Client.SCR as SCR
import qualified Constants
import           Game.CVarT
import qualified Game.GameBase as GameBase
import qualified Game.GameSpawn as GameSpawn
import qualified QCommon.CBuf as CBuf
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import qualified QCommon.SZ as SZ
import           QuakeState
import           Server.ClientT
import           Server.ServerStaticT
import           Server.ServerT
import qualified Server.SVSend as SVSend
import qualified Server.SVWorld as SVWorld
import           Types
import           Util.Binary (encode)
import qualified Util.Lib as Lib

import           Control.Lens (use, zoom, (^.), (.=), (+=), (%=), (&), (.~), (%~))
import           Control.Monad (void, when)
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
          do void (CVar.set "nextserver" "")
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
        checkSpawnPoint Nothing = ("", level)
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
initGame = error "SVInit.initGame" -- TODO

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
     (modelIdx, iw) <- loadContent
     svGlobals.svServer.sModels %= (V.// [(1, Ref modelIdx)])
     svGlobals.svServer.sConfigStrings %= (V.// [(Constants.csMapChecksum, encode (head iw))])
  where loadContent | srvState /= Constants.ssGame = CM.loadMap "" False [0] -- no real map
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
createBaseline = error "SVInit.createBaseline" -- TODO

checkForSavegame :: Quake ()
checkForSavegame = error "SVInit.checkForSavegame" -- TODO
