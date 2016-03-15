{-# LANGUAGE FlexibleContexts #-}
module Server.SVMain
  ( dropClient
  , frame
  , initialize
  , shutdown
  ) where

import qualified Constants
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import           Game.GClientT
import           Game.PlayerStateT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import           QCommon.NetAdrT
import qualified QCommon.NetChannel as NetChannel
import qualified QCommon.SZ as SZ
import           QuakeRef
import           QuakeState
import           Server.ClientT
import           Server.ServerStaticT
import           Server.ServerT
import qualified Server.SVConsoleCommands as SVConsoleCommands
import qualified Server.SVEnts as SVEnts
import           Server.SVMainShared
import qualified Server.SVSend as SVSend
import qualified Sys.NET as NET
import           Types
import           Util.Binary (encode)
import qualified Util.Lib as Lib

import           Control.Lens (use, (^.), (.=), (+=), (%=), (&), (.~))
import           Control.Monad (void, when)
import           Data.Bits ((.|.), (.&.))
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

heartbeatSeconds :: Int
heartbeatSeconds = 300

initialize :: Quake ()
initialize =
  do SVConsoleCommands.initOperatorCommands
     CVar.initializeCVars initialCVars
     SZ.initialize (globals.gNetMessage) B.empty Constants.maxMsgLen

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars =
  [ ("rcon_password", B.empty, 0)
  , ("skill", "1", 0)
  , ("deathmatch", "0", Constants.cvarLatch)
  , ("coop", "0", Constants.cvarLatch)
  , ("dmflags", encode Constants.dfInstantItems, Constants.cvarServerInfo)
  , ("fraglimit", "0", Constants.cvarServerInfo)
  , ("timelimit", "0", Constants.cvarServerInfo)
  , ("cheats", "0", Constants.cvarServerInfo .|. Constants.cvarLatch)
  , ("protocol", encode Constants.protocolVersion, Constants.cvarServerInfo .|. Constants.cvarNoSet)
  , ("maxclients", "1", Constants.cvarServerInfo .|. Constants.cvarLatch)
  , ("hostname", "noname", Constants.cvarServerInfo .|. Constants.cvarArchive)
  , ("timeout", "125", 0)
  , ("zombietime", "2", 0)
  , ("showclamp", "0", 0)
  , ("paused", "0", 0)
  , ("timedemo", "0", 0)
  , ("sv_enforcetime", "0", 0)
  , ("allow_download", "1", Constants.cvarArchive)
  , ("allow_download_players", "0", Constants.cvarArchive)
  , ("allow_download_models", "1", Constants.cvarArchive)
  , ("allow_download_sounds", "1", Constants.cvarArchive)
  , ("allow_download_maps", "1", Constants.cvarArchive)
  , ("sv_noreload", "0", 0)
  , ("sv_airaccelerate", "0", Constants.cvarLatch)
  , ("public", "0", 0)
  , ("sv_reconnect_limit", "3", Constants.cvarArchive)
  ]

frame :: Int -> Quake ()
frame msec =
  do globals.gTimeBeforeGame .= 0
     globals.gTimeAfterGame .= 0
     runFrame msec =<< use (svGlobals.svServerStatic.ssInitialized)

runFrame :: Int -> Bool -> Quake ()
runFrame _ False = return ()
runFrame msec True =
  do svGlobals.svServerStatic.ssRealTime += msec
     void Lib.rand
     checkTimeouts
     readPackets
     timeDemo <- timeDemoCVar
     realTime <- use (svGlobals.svServerStatic.ssRealTime)
     time <- use (svGlobals.svServer.sTime)
     checkTimeShift timeDemo realTime time

checkTimeShift :: CVarT -> Int -> Int -> Quake ()
checkTimeShift timeDemo realTime time
  | (timeDemo^.cvValue) == 0 && realTime < time =
      do when (time - realTime > 100) $
           rollBackRealTime =<< showClampCVar
         sleep =<< use (svGlobals.svServerStatic.ssRealTime)
  | otherwise =
      do calcPings
         giveMsec
         runGameFrame
         SVSend.sendClientMessages
         SVEnts.recordDemoMessage
         masterHeartbeat
         prepWorldFrame
  where rollBackRealTime showClamp =
          do when ((showClamp^.cvValue) /= 0) $
               Com.printf "sv lowclamp\n"
             svGlobals.svServerStatic.ssRealTime .= time - 100
        sleep updatedRealTime = NET.sleep (time - updatedRealTime)

checkTimeouts :: Quake ()
checkTimeouts = error "SVMain.checkTimeouts" -- TODO

readPackets :: Quake ()
readPackets = error "SVMain.readPackets" -- TODO

calcPings :: Quake ()
calcPings = error "SVMain.calcPings" -- TODO

giveMsec :: Quake ()
giveMsec = checkAndGive =<< use (svGlobals.svServer.sFrameNum)
  where checkAndGive frameNum
          | frameNum .&. 15 == 0 =
              svGlobals.svServerStatic.ssClients %= fmap giveTime
          | otherwise = return ()
        giveTime client
          | (client^.cState) == Constants.csFree = client
          | otherwise = client & cCommandMsec .~ 1800 -- 1600 + some slop

runGameFrame :: Quake ()
runGameFrame = error "SVMain.runGameFrame" -- TODO

masterHeartbeat :: Quake ()
masterHeartbeat =
  do dedicated <- dedicatedCVar
     publicServer <- publicServerCVar
     sendMasterHeartbeat dedicated publicServer
  where sendMasterHeartbeat dedicated publicServer
          | (dedicated^.cvValue) /= 0 || (publicServer^.cvValue) == 0 =
              do lastHeartbeat <- use (svGlobals.svServerStatic.ssLastHeartbeat)
                 realTime <- use (svGlobals.svServerStatic.ssRealTime)
                 doSendHeartbeat lastHeartbeat realTime
          | otherwise = return ()

doSendHeartbeat :: Int -> Int -> Quake ()
doSendHeartbeat lastHeartbeat realTime
  | lastHeartbeat > realTime = svGlobals.svServerStatic.ssLastHeartbeat .= realTime
  | realTime - lastHeartbeat < heartbeatSeconds * 1000 = return ()
  | otherwise =
      do svGlobals.svServerStatic.ssLastHeartbeat .= realTime
         str <- statusString
         masterAdr <- use (svGlobals.svMasterAdr)
         mapM_ (sendToNetAdr str) (V.filter ((/= 0) . (^.naPort)) masterAdr)

sendToNetAdr :: B.ByteString -> NetAdrT -> Quake ()
sendToNetAdr msg netAdr =
  do Com.printf (B.concat ["Sending heartbeat to ", NET.adrToString netAdr, "\n"])
     NetChannel.outOfBandPrint Constants.nsServer netAdr ("heartbeat\n" `B.append` msg)

prepWorldFrame :: Quake ()
prepWorldFrame =
  do numEdicts <- use (gameBaseGlobals.gbNumEdicts)
     mapM_ resetEdictEvent [0..numEdicts-1]
  where resetEdictEvent idx =
          modifyRef (Ref idx) (\v -> v & eEntityState.esEvent .~ 0)

statusString :: Quake B.ByteString
statusString =
  do status <- fmap (`B.append` "\n") CVar.serverInfo
     maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
     clients <- use (svGlobals.svServerStatic.ssClients)
     V.foldM collectStatusString status (V.take maxClients clients)

collectStatusString :: B.ByteString -> ClientT -> Quake B.ByteString
collectStatusString acc client
  | clientInGame =
      do gClient <- getGClient
         return (appendClientInfo acc client gClient)
  | otherwise = return acc
  where clientInGame = (client^.cState) `elem` [Constants.csConnected, Constants.csSpawned]
        getGClient =
          do edictRef <- maybe edictError return (client^.cEdict)
             edict <- readRef edictRef
             readRef =<< maybe gClientError return (edict^.eClient)
        edictError =
          do Com.fatalError "SVMain.collectStatusString client^.cEdict is Nothing"
             return (Ref (-1))
        gClientError =
          do Com.fatalError "SVMain.collectStatusString edict^.eClient is Nothing"
             return (Ref (-1))

appendClientInfo :: B.ByteString -> ClientT -> GClientT -> B.ByteString
appendClientInfo acc client gClient
  | B.length acc + B.length player >= 1024 = acc
  | otherwise = acc `B.append` player
  where player = B.concat [ encode ((gClient^.gcPlayerState.psStats) UV.! Constants.statFrags)
                          , " ", encode (client^.cPing), "\"", client^.cName, "\"\n"
                          ]