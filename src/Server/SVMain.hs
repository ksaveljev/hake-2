{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server.SVMain where

import Control.Lens (use, preuse, (.=), (%=), (^.), (+=), Traversal', ix)
import Control.Monad (void, when, liftM, unless)
import Data.Bits ((.|.), (.&.))
import Data.Maybe (isJust)
import Data.Traversable (traverse)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Game.GameBase as GameBase
import qualified Game.PlayerClient as PlayerClient
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.MSG as MSG
import qualified QCommon.NetChannel as NetChannel
import qualified QCommon.SZ as SZ
import {-# SOURCE #-} qualified Server.SVConsoleCommands as SVConsoleCommands
import qualified Server.SVEnts as SVEnts
import qualified Server.SVGame as SVGame
import {-# SOURCE #-} qualified Server.SVSend as SVSend
import qualified Server.SVUser as SVUser
import qualified Sys.NET as NET
import qualified Sys.Timer as Timer
import qualified Util.Lib as Lib

{-
- Send a message to the master every few minutes to let it know we are
- alive, and log information.
-}
heartbeatSeconds :: Int
heartbeatSeconds = 300

-- only called at quake2.exe startup, not for each game
init :: Quake ()
init = do
    SVConsoleCommands.initOperatorCommands

    void $ CVar.get "rcon_password" "" 0

    void $ CVar.get "skill" "1" 0
    void $ CVar.get "deathmatch" "0" Constants.cvarLatch
    void $ CVar.get "coop" "0" Constants.cvarLatch
    void $ CVar.get "dmflags" (BC.pack $ show Constants.dfInstantItems) Constants.cvarServerInfo -- IMPROVE: convert Int to ByteString using binary package?
    void $ CVar.get "fraglimit" "0" Constants.cvarServerInfo
    void $ CVar.get "timelimit" "0" Constants.cvarServerInfo
    void $ CVar.get "cheats" "0" (Constants.cvarServerInfo .|. Constants.cvarLatch)
    void $ CVar.get "protocol" (BC.pack $ show Constants.protocolVersion) (Constants.cvarServerInfo .|. Constants.cvarNoSet) -- IMPROVE: convert Int to ByteString using binary package?

    void $ CVar.get "maxclients" "1" (Constants.cvarServerInfo .|. Constants.cvarLatch)
    void $ CVar.get "hostname" "noname" (Constants.cvarServerInfo .|. Constants.cvarArchive)
    void $ CVar.get "timeout" "125" 0
    void $ CVar.get "zombietime" "2" 0
    void $ CVar.get "showclamp" "0" 0
    void $ CVar.get "paused" "0" 0
    void $ CVar.get "timedemo" "0" 0
    void $ CVar.get "sv_enforcetime" "0" 0
    void $ CVar.get "allow_download" "1" Constants.cvarArchive
    void $ CVar.get "allow_download_players" "0" Constants.cvarArchive
    void $ CVar.get "allow_download_models" "1" Constants.cvarArchive
    void $ CVar.get "allow_download_sounds" "1" Constants.cvarArchive
    void $ CVar.get "allow_download_maps" "1" Constants.cvarArchive
    void $ CVar.get "sv_noreload" "0" 0
    void $ CVar.get "sv_airaccelerate" "0" Constants.cvarLatch
    void $ CVar.get "public" "0" 0
    void $ CVar.get "sv_reconnect_limit" "3" Constants.cvarArchive

    bufData <- use $ globals.netMessageBuffer
    SZ.init (globals.netMessage) bufData Constants.maxMsgLen

-- Called when each game quits, before Sys_Quit or Sys_Error.
shutdown :: B.ByteString -> Bool -> Quake ()
shutdown finalmsg reconnect = do
    clients <- use $ svGlobals.svServerStatic.ssClients

    when (V.null clients) $ 
      finalMessage finalmsg reconnect

    masterShutdown

    SVGame.shutdownGameProgs

    -- free current level
    sDemofile <- use $ svGlobals.svServer.sDemoFile
    when (isJust sDemofile) $ do
      let Just h = sDemofile
      Lib.fClose h

    let newServer = newServerT
    svGlobals.svServer .= newServer
    globals.serverState .= (newServer^.sState)

    ssDemofile <- use $ svGlobals.svServerStatic.ssDemoFile
    when (isJust ssDemofile) $ do
      let Just h = ssDemofile
      Lib.fClose h

    svGlobals.svServerStatic .= newServerStaticT

{-
- Used by SV_Shutdown to send a final message to all connected clients
- before the server goes down. The messages are sent immediately, not just
- stuck on the outgoing message list, because the server is going to
- totally exit after returning from this function.
-}
finalMessage :: B.ByteString -> Bool -> Quake ()
finalMessage _ _ = io (putStrLn "SVMain.finalMessage") >> undefined -- TODO

-- Master_Shutdown, Informs all masters that this server is going down.
masterShutdown :: Quake ()
masterShutdown = io (putStrLn "SVMain.masterShutdown") >> undefined -- TODO

{-
- Called when the player is totally leaving the server, either willingly or
- unwillingly. This is NOT called if the entire server is quiting or
- crashing.
-}
dropClient :: Traversal' QuakeState ClientT -> Quake ()
dropClient clientLens = do
    Just client <- preuse $ clientLens

    MSG.writeByteI (clientLens.cNetChan.ncMessage) Constants.svcDisconnect

    when ((client^.cState) == Constants.csSpawned) $
      PlayerClient.clientDisconnect (clientLens.cEdict)

    when (isJust (client^.cDownload)) $
      clientLens.cDownload .= Just ""

    clientLens.cState .= Constants.csZombie
    clientLens.cName .= ""

{- ==============================================================================
- 
- CONNECTIONLESS COMMANDS
- 
- ==============================================================================-}

{-
 - Builds the string that is sent as heartbeats and status replies.
 -}
statusString :: Quake B.ByteString
statusString = io (putStrLn "SVMain.statusString") >> undefined -- TODO

frame :: Int -> Quake ()
frame msec = do
    globals.timeBeforeGame .= 0
    globals.timeAfterGame .= 0

    -- if server is not active, do nothing
    initialized <- use $ svGlobals.svServerStatic.ssInitialized

    when initialized $ do
      svGlobals.svServerStatic.ssRealTime += msec

      -- keep the random time dependent
      void Lib.rand

      -- check timeouts
      checkTimeouts

      -- get packets from clients
      readPackets

      -- move autonomous things around if enough time has passed
      timeDemoValue <- liftM (^.cvValue) svTimeDemoCVar
      realTime <- use $ svGlobals.svServerStatic.ssRealTime
      time <- use $ svGlobals.svServer.sTime

      if timeDemoValue == 0 && realTime < time
        then do
          when (time - realTime > 100) $ do
            showClampValue <- liftM (^.cvValue) svShowClampCVar

            when (showClampValue /= 0) $
              Com.printf "sv lowclamp\n"
            svGlobals.svServerStatic.ssRealTime .= time - 100

          updatedRealTime <- use $ svGlobals.svServerStatic.ssRealTime
          NET.sleep (time - updatedRealTime)

        else do
          -- update ping based on the last known frame from all clients
          calcPings

          -- give the clients some timeslices
          giveMsec

          -- let everything in the world think and move
          runGameFrame

          -- send messages back to the clients that had packets read this frame
          SVSend.sendClientMessages

          -- save the entire world state if recording a serverdemo
          SVEnts.recordDemoMessage

          -- send a heartbeat to the master if needed
          masterHeartbeat

          -- clear teleport flags, etc for next frame
          prepWorldFrame

{-
- If a packet has not been received from a client for timeout.value
- seconds, drop the conneciton. Server frames are used instead of realtime
- to avoid dropping the local client while debugging.
- 
- When a client is normally dropped, the client_t goes into a zombie state
- for a few seconds to make sure any final reliable message gets resent if
- necessary.
-}
checkTimeouts :: Quake ()
checkTimeouts = do
    realTime <- use $ svGlobals.svServerStatic.ssRealTime
    timeoutValue <- liftM (^.cvValue) timeoutCVar
    zombieTimeValue <- liftM (^.cvValue) zombieTimeCVar

    let dropPoint = realTime - truncate (1000 * timeoutValue)
        zombiePoint = realTime - truncate (1000 * zombieTimeValue)

    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar

    checkClientTimeout realTime dropPoint zombiePoint 0 maxClientsValue

  where checkClientTimeout :: Int -> Int -> Int -> Int -> Int -> Quake ()
        checkClientTimeout realTime dropPoint zombiePoint idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just lastMessage <- preuse $ svGlobals.svServerStatic.ssClients.ix idx.cLastMessage

              -- message times may be wrong across a changelevel
              when (lastMessage > realTime) $
                svGlobals.svServerStatic.ssClients.ix idx.cLastMessage .= realTime

              Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix idx

              if (client^.cState) == Constants.csZombie && (client^.cLastMessage) < zombiePoint
                then
                  svGlobals.svServerStatic.ssClients.ix idx.cState .= Constants.csFree -- can now be reused
                else
                  when (((client^.cState) == Constants.csConnected || (client^.cState) == Constants.csSpawned) && (client^.cLastMessage) < dropPoint) $ do
                    SVSend.broadcastPrintf Constants.printHigh $ (client^.cName) `B.append` " timed out\n"
                    dropClient (svGlobals.svServerStatic.ssClients.ix idx)
                    svGlobals.svServerStatic.ssClients.ix idx.cState .= Constants.csFree -- don't bother with zombie state

              checkClientTimeout realTime dropPoint zombiePoint (idx + 1) maxIdx

-- Reads packets from the network or loopback
readPackets :: Quake ()
readPackets = do
    readSomething <- NET.getPacket Constants.nsServer (globals.netFrom) (globals.netMessage)

    when readSomething $ do
      -- check for connectionless packet (0xffffffff) first
      netMessageData <- liftM (B.take 4) (use $ globals.netMessage.sbData)
      if netMessageData == B.pack [0xFF, 0xFF, 0xFF, 0xFF]
        then do
          connectionlessPacket
          readPackets
        else do
          -- read the qport out of the message so we can fix up
          -- stupid address translating routers
          MSG.beginReading (globals.netMessage)
          void $ MSG.readLong (globals.netMessage) -- sequence number
          void $ MSG.readLong (globals.netMessage) -- sequence number
          qport <- MSG.readShort (globals.netMessage)

          -- check for packets from connected clients
          maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar
          idx <- checkClientPackets (fromIntegral qport) 0 maxClientsValue

          unless (idx >= maxClientsValue) $
            readPackets

  where checkClientPackets :: Int -> Int -> Int -> Quake Int
        checkClientPackets qport idx maxIdx
          | idx >= maxIdx = return idx
          | otherwise = do
              Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix idx
              netAdr <- use $ globals.netFrom

              -- IMPROVE: first 3 statements can be squashed into one?
              if | (client^.cState) == Constants.csFree -> checkClientPackets qport (idx + 1) maxIdx
                 | not (NET.compareBaseAdr netAdr (client^.cNetChan.ncRemoteAddress)) -> checkClientPackets qport (idx + 1) maxIdx
                 | (client^.cNetChan.ncRemoteQPort) /= qport -> checkClientPackets qport (idx + 1) maxIdx
                 | otherwise -> do
                     when ((client^.cNetChan.ncRemoteAddress.naPort) /= (netAdr^.naPort)) $ do
                       Com.printf "SV_ReadPackets: fixing up a translated port\n"
                       svGlobals.svServerStatic.ssClients.ix idx.cNetChan.ncRemoteAddress.naPort .= (netAdr^.naPort)

                     ok <- NetChannel.process (svGlobals.svServerStatic.ssClients.ix idx.cNetChan) (globals.netMessage)
                     when ok $
                       -- this is a valid, sequenced packet, so process it
                       when ((client^.cState) /= Constants.csZombie) $ do
                         realTime <- use $ svGlobals.svServerStatic.ssRealTime
                         svGlobals.svServerStatic.ssClients.ix idx.cLastMessage .= realTime -- don't timeout
                         SVUser.executeClientMessage idx

                     return idx

-- Updates the cl.ping variables
calcPings :: Quake ()
calcPings = do
    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar
    updateClientPing 0 maxClientsValue

  where updateClientPing :: Int -> Int -> Quake ()
        updateClientPing idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix idx

              when ((client^.cState) == Constants.csSpawned) $ do
                (count, total) <- calcFrameLatency idx 0 0 0 Constants.latencyCounts

                let ping = if count == 0 then 0 else total `div` count
                svGlobals.svServerStatic.ssClients.ix idx.cPing .= ping
                
                -- let the game dll know about the ping
                let Just (EdictReference edictIdx) = client^.cEdict
                Just gclient <- preuse $ gameBaseGlobals.gbGEdicts.ix edictIdx.eClient
                let Just (GClientReference gclientIdx) = gclient
                gameBaseGlobals.gbGame.glClients.ix gclientIdx.gcPing .= ping

              updateClientPing (idx + 1) maxIdx

        calcFrameLatency :: Int -> Int -> Int -> Int -> Int -> Quake (Int, Int)
        calcFrameLatency clientIdx count total idx maxIdx
          | idx >= maxIdx = return (count, total)
          | otherwise = do
              Just frameLatency <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx.cFrameLatency.ix idx
              if frameLatency > 0
                then calcFrameLatency clientIdx (count + 1) (total + frameLatency) (idx + 1) maxIdx
                else calcFrameLatency clientIdx count total (idx + 1) maxIdx

{-
- Every few frames, gives all clients an allotment of milliseconds for
- their command moves. If they exceed it, assume cheating.
-}
giveMsec :: Quake ()
giveMsec = do
    frameNum <- use $ svGlobals.svServer.sFrameNum

    when (frameNum .&. 15 == 0) $
      svGlobals.svServerStatic.ssClients %=
        fmap (\client -> if (client^.cState) == Constants.csFree
                      then client
                      else client { _cCommandMsec = 1800 }) -- 1600 + some slop


runGameFrame :: Quake ()
runGameFrame = do
    setTimeBeforeGame

    -- we always need to bump framenum, even if we
    -- don't run the world, otherwise the delta
    -- compression can get confused when a client
    -- has the "current" frame
    svGlobals.svServer.sFrameNum += 1
    frameNum <- use $ svGlobals.svServer.sFrameNum
    svGlobals.svServer.sTime .= frameNum * 100

    -- don't run if paused
    pausedValue <- liftM (^.cvValue) svPausedCVar
    maxClientsValue <- liftM (^.cvValue) maxClientsCVar

    when (pausedValue == 0 || maxClientsValue > 1) $ do
      GameBase.runFrame

      -- never get more than one tic behind
      time <- use $ svGlobals.svServer.sTime
      realTime <- use $ svGlobals.svServerStatic.ssRealTime
      when (time < realTime) $ do
        showClampValue <- liftM (^.cvValue) svShowClampCVar

        when (showClampValue /= 0) $
          Com.printf "sv highclamp\n"

        svGlobals.svServerStatic.ssRealTime .= time

    setTimeAfterGame

  where setTimeBeforeGame :: Quake ()
        setTimeBeforeGame = do
          hostSpeedsValue <- liftM (^.cvValue) hostSpeedsCVar

          when (hostSpeedsValue /= 0) $ do
            t <- Timer.milliseconds
            globals.timeBeforeGame .= t

        setTimeAfterGame :: Quake ()
        setTimeAfterGame = do
          hostSpeedsValue <- liftM (^.cvValue) hostSpeedsCVar

          when (hostSpeedsValue /= 0) $ do
            t <- Timer.milliseconds
            globals.timeAfterGame .= t

masterHeartbeat :: Quake ()
masterHeartbeat = do
    dedicatedValue <- liftM (^.cvValue) dedicatedCVar
    publicServerValue <- liftM (^.cvValue) publicServerCVar


    -- only dedicated servers send heartbeats
    -- no need if it is a private dedicated game
    when (dedicatedValue /= 0 || publicServerValue == 0) $ do
      lastHeartbeat <- use $ svGlobals.svServerStatic.ssLastHeartbeat
      realtime <- use $ svGlobals.svServerStatic.ssRealTime

      if | lastHeartbeat > realtime -> svGlobals.svServerStatic.ssLastHeartbeat .= realtime
         | realtime - lastHeartbeat < heartbeatSeconds * 1000 -> return () -- not time to send yet
         | otherwise -> do
             svGlobals.svServerStatic.ssLastHeartbeat .= realtime

             -- send the same string that we would give for a status OOB command
             str <- statusString

             -- send to group master
             masterAdr <- use $ svGlobals.svMasterAdr
             void $ traverse (sendToNetAdr str) masterAdr

  where sendToNetAdr :: B.ByteString -> NetAdrT -> Quake ()
        sendToNetAdr strToSend netAdr = do
          Com.printf $ "Sending heartbeat to " `B.append` NET.adrToString netAdr `B.append` "\n"
          NetChannel.outOfBandPrint Constants.nsServer netAdr ("heartbeat\n" `B.append` strToSend)

{-
- SV_PrepWorldFrame
- 
- This has to be done before the world logic, because player processing
- happens outside RunWorldFrame.
-}
prepWorldFrame :: Quake ()
prepWorldFrame = io (putStrLn "SVMain.prepWorldFrame") >> undefined -- TODO

connectionlessPacket :: Quake ()
connectionlessPacket = io (putStrLn "SVMain.connectionlessPacket") >> undefined -- TODO
