{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server.SVMain where

import Control.Lens (use, preuse, (.=), (%=), (^.), (+=), ix, zoom, (&), (.~))
import Control.Monad (void, when, liftM, unless)
import Data.Bits ((.|.), (.&.))
import Data.Maybe (isJust, fromJust)
import Data.Traversable (traverse)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Types
import QuakeState
import CVarVariables
import qualified Constants
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified Game.GameBase as GameBase
import qualified Game.Info as Info
import qualified Game.PlayerClient as PlayerClient
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.MSG as MSG
import qualified QCommon.NetChannel as NetChannel
import qualified QCommon.SZ as SZ
import {-# SOURCE #-} qualified Server.SVConsoleCommands as SVConsoleCommands
import qualified Server.SVEnts as SVEnts
import qualified Server.SVGame as SVGame
import {-# SOURCE #-} qualified Server.SVSend as SVSend
import {-# SOURCE #-} qualified Server.SVUser as SVUser
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
masterShutdown = do
    dedicatedValue <- liftM (^.cvValue) dedicatedCVar
    publicServerValue <- liftM (^.cvValue) publicServerCVar

    unless (dedicatedValue == 0 || publicServerValue == 0) $ do
      -- send to group master
      io (putStrLn "SVMain.masterShutdown") >> undefined -- TODO

{-
- Called when the player is totally leaving the server, either willingly or
- unwillingly. This is NOT called if the entire server is quiting or
- crashing.
-}
dropClient :: ClientReference -> Quake ()
dropClient (ClientReference clientIdx) = do
    Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx

    MSG.writeByteI (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan.ncMessage) (fromIntegral Constants.svcDisconnect)

    when ((client^.cState) == Constants.csSpawned) $
      PlayerClient.clientDisconnect (fromJust $ client^.cEdict)

    when (isJust (client^.cDownload)) $
      svGlobals.svServerStatic.ssClients.ix clientIdx.cDownload .= Just ""

    zoom (svGlobals.svServerStatic.ssClients.ix clientIdx) $ do
      cState .= Constants.csZombie
      cName .= ""

{- ==============================================================================
- 
- CONNECTIONLESS COMMANDS
- 
- ==============================================================================-}

{-
 - Builds the string that is sent as heartbeats and status replies.
 -}
statusString :: Quake B.ByteString
statusString = do
    status <- liftM (`B.append` "\n") CVar.serverInfo
    maxClientsValue <- liftM (truncate . (^.cvValue)) maxClientsCVar

    collectStatusString 0 maxClientsValue status

  where collectStatusString :: Int -> Int -> B.ByteString -> Quake B.ByteString
        collectStatusString idx maxIdx acc
          | idx >= maxIdx = return acc
          | otherwise = do
              Just client <- preuse $ svGlobals.svServerStatic.ssClients.ix idx
              if (client^.cState) == Constants.csConnected || (client^.cState) == Constants.csSpawned
                then do
                  let Just edictRef = client^.cEdict
                  gClientRef <- readEdictT edictRef >>= \e -> return (e^.eClient)
                  let Just (GClientReference gClientIdx) = gClientRef
                  Just clientStats <- preuse $ gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPlayerState.psStats

                  let player = BC.pack (show $ clientStats UV.! Constants.statFrags) `B.append`
                               " " `B.append` BC.pack (show $ client^.cPing) `B.append`
                               "\"" `B.append` (client^.cName) `B.append` "\"\n"

                  if B.length acc + B.length player >= 1024
                    then -- can't hold any more
                      return acc
                    else
                      collectStatusString (idx + 1) maxIdx (acc `B.append` player)

                else collectStatusString (idx + 1) maxIdx acc

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
                    dropClient (ClientReference idx)
                    svGlobals.svServerStatic.ssClients.ix idx.cState .= Constants.csFree -- don't bother with zombie state

              checkClientTimeout realTime dropPoint zombiePoint (idx + 1) maxIdx

remoteCommandHeader :: B.ByteString
remoteCommandHeader = B.pack [0xFF, 0xFF, 0xFF, 0xFF]

-- Reads packets from the network or loopback
readPackets :: Quake ()
readPackets = do
    readSomething <- NET.getPacket Constants.nsServer (globals.netFrom) (globals.netMessage)

    when readSomething $ do
      -- check for connectionless packet (0xffffffff) first
      netMessageHeader <- liftM (B.take 4) (use $ globals.netMessage.sbData)
      if netMessageHeader == remoteCommandHeader
        then do
          connectionlessPacket
          readPackets
        else do
          -- read the qport out of the message so we can fix up
          -- stupid address translating routers
          MSG.beginReading (globals.netMessage)
          void $ MSG.readLong (globals.netMessage) -- sequence number
          void $ MSG.readLong (globals.netMessage) -- sequence number
          qport <- liftM (.&. 0xFFFF) (MSG.readShort (globals.netMessage))

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
                         SVUser.executeClientMessage (ClientReference idx)

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
                let Just edictRef = client^.cEdict
                gClientRef <- readEdictT edictRef >>= \e -> return (e^.eClient)
                let Just (GClientReference gClientIdx) = gClientRef
                gameBaseGlobals.gbGame.glClients.ix gClientIdx.gcPing .= ping

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
    io (print "runGameFrame")
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
             void $ traverse (sendToNetAdr str) (V.filter (\a -> (a^.naPort) /= 0) masterAdr)

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
prepWorldFrame = do
    numEdicts <- use $ gameBaseGlobals.gbNumEdicts
    resetEdictEvent 0 numEdicts

  where resetEdictEvent :: Int -> Int -> Quake ()
        resetEdictEvent idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              -- events only last for a single message
              modifyEdictT (newEdictReference idx) (\v -> v & eEntityState.esEvent .~ 0)
              resetEdictEvent (idx + 1) maxIdx

{-
- A connectionless packet has four leading 0xff characters to distinguish
- it from a game channel. Clients that are in the game can still send
- connectionless packets. It is used also by rcon commands.
-}
connectionlessPacket :: Quake ()
connectionlessPacket = do
    MSG.beginReading (globals.netMessage)
    _ <- MSG.readLong (globals.netMessage) -- skip the -1 marker

    s <- MSG.readStringLine (globals.netMessage)

    Cmd.tokenizeString s False

    c <- Cmd.argv 0
    
    from <- use $ globals.netFrom

    case c of
      "ping" -> svcPing
      "ack" -> svcAck
      "status" -> svcStatus
      "info" -> svcInfo
      "getchallenge" -> svcGetChallenge
      "connect" -> svcDirectConnect
      "rcon" -> svcRemoteCommand
      _ -> do
        Com.printf $ "bad connectionless packet from " `B.append` NET.adrToString from `B.append` "\n"
        Com.printf $ "[" `B.append` s `B.append` "]\n"
        -- TODO: print hexdump of data like in jake2?

svcPing :: Quake ()
svcPing = do
    from <- use $ globals.netFrom
    NetChannel.outOfBandPrint Constants.nsServer from "ack"

svcAck :: Quake ()
svcAck = do
    from <- use $ globals.netFrom
    Com.printf $ "Ping acknowledge from " `B.append` NET.adrToString from `B.append` "\n"

svcStatus :: Quake ()
svcStatus = do
    status <- statusString
    from <- use $ globals.netFrom
    NetChannel.outOfBandPrint Constants.nsServer from ("print\n" `B.append` status)

svcInfo :: Quake ()
svcInfo = io (putStrLn "SVMain.svcInfo") >> undefined -- TODO

{-
- Returns a challenge number that can be used in a subsequent
- client_connect command. We do this to prevent denial of service attacks
- that flood the server with invalid connection IPs. With a challenge, they
- must give a valid IP address.
-}
svcGetChallenge :: Quake ()
svcGetChallenge = do
    -- see if we already have a challenge for this ip
    adr <- use $ globals.netFrom
    challenges <- use $ svGlobals.svServerStatic.ssChallenges
    (i, oldest) <- challengeExists adr challenges 0 0x7FFFFFFF 0 Constants.maxChallenges

    if i == Constants.maxChallenges
      then do
        -- overwrite the oldest
        r <- Lib.rand
        curTime' <- Timer.getCurTime
        svGlobals.svServerStatic.ssChallenges.ix oldest .= ChallengeT adr (fromIntegral r .&. 0x7FFFF) curTime'

        -- send it back
        NetChannel.outOfBandPrint Constants.nsServer adr ("challenge " `B.append` BC.pack (show ((challenges V.! oldest)^.chChallenge))) -- IMPROVE

      else
        -- send it back
        NetChannel.outOfBandPrint Constants.nsServer adr ("challenge " `B.append` BC.pack (show ((challenges V.! i)^.chChallenge))) -- IMPROVE

  where challengeExists :: NetAdrT -> V.Vector ChallengeT -> Int -> Int -> Int -> Int -> Quake (Int, Int)
        challengeExists adr challenges oldest oldestTime idx maxIdx
          | idx >= maxIdx = return (idx, oldest)
          | otherwise = do
              let challenge = challenges V.! idx
              if NET.compareBaseAdr adr (challenge^.chAdr)
                then
                  return (idx, oldest)
                else 
                  if (challenge^.chTime) < oldestTime
                    then challengeExists adr challenges idx (challenge^.chTime) (idx + 1) maxIdx
                    else challengeExists adr challenges oldest oldestTime (idx + 1) maxIdx

-- A connection request that did not come from the master.
svcDirectConnect :: Quake ()
svcDirectConnect = do
    adr <- use $ globals.netFrom

    Com.printf "SVC_DirectConnect ()\n"

    version <- Cmd.argv 1 >>= return . Lib.atoi

    if version /= Constants.protocolVersion
      then do
        NetChannel.outOfBandPrint Constants.nsServer adr ("print\nServer is version " `B.append` BC.pack (show Constants.version) `B.append` "\n") -- IMPROVE ?
        Com.dprintf $ "    rejected connect from version " `B.append` BC.pack (show version) `B.append` "\n" -- IMPROVE ?
      else do
        qport <- Cmd.argv 2 >>= return . Lib.atoi
        challenge <- Cmd.argv 3 >>= return . Lib.atoi
        -- force the IP key/value pair so the game can filter based on ip
        userInfo <- Cmd.argv 4 >>= \v -> Info.setValueForKey v "ip" (NET.adrToString adr)

        -- attractloop servers are ONLY for local clients
        attractloop <- use $ svGlobals.svServer.sAttractLoop
        if attractloop && not (NET.isLocalAddress adr)
          then do
            Com.printf "Remote connect in attract loop.  Ignored.\n"
            NetChannel.outOfBandPrint Constants.nsServer adr "print\nConnection refused.\n"
          else do
            -- see if the challenge is valid
            ok <- if not (NET.isLocalAddress adr)
                    then do
                      challenges <- use $ svGlobals.svServerStatic.ssChallenges
                      let foundChallenge = V.find (\c -> NET.compareBaseAdr adr (c^.chAdr)) challenges

                      case foundChallenge of
                        Nothing -> do
                          NetChannel.outOfBandPrint Constants.nsServer adr "print\nNo challenge for address.\n"
                          return False
                        Just ch -> do
                          if (ch^.chChallenge) == challenge
                            then
                              return True
                            else do
                              NetChannel.outOfBandPrint Constants.nsServer adr "print\nBad challenge.\n"
                              return False

                    else
                      return True

            when ok $ do
              -- if there is already a slot for this ip, reuse it
              maxClientsValue :: Int <- liftM (truncate . (^.cvValue)) maxClientsCVar
              clients <- liftM (V.take maxClientsValue) (use $ svGlobals.svServerStatic.ssClients)
              done <- findAndReuseIPSlot clients adr qport challenge userInfo 0 maxClientsValue

              unless done $ do
                -- find a client slot
                let foundClientSlot = V.findIndex (\c -> c^.cState == Constants.csFree) clients
                case foundClientSlot of
                  Nothing -> do
                    NetChannel.outOfBandPrint Constants.nsServer adr "print\nServer is full.\n"
                    Com.dprintf "Rejected a connection.\n"
                  Just idx -> do
                    gotNewClient (ClientReference idx) challenge userInfo adr qport

  where findAndReuseIPSlot :: V.Vector ClientT -> NetAdrT -> Int -> Int -> B.ByteString -> Int -> Int -> Quake Bool
        findAndReuseIPSlot clients adr qport challenge userInfo idx maxIdx
          | idx >= maxIdx = return False -- no existing slot has been found for client ip
          | otherwise = do
              let client = clients V.! idx

              if client^.cState == Constants.csFree
                then findAndReuseIPSlot clients adr qport challenge userInfo (idx + 1) maxIdx
                else do
                  if NET.compareBaseAdr adr (client^.cNetChan.ncRemoteAddress) && ((client^.cNetChan.ncRemoteQPort) == qport || (adr^.naPort) == (client^.cNetChan.ncRemoteAddress.naPort))
                    then do
                      realTime <- use $ svGlobals.svServerStatic.ssRealTime
                      reconnectLimitValue <- liftM (^.cvValue) svReconnectLimitCVar

                      if (not (NET.isLocalAddress adr)) && (realTime - (client^.cLastConnect) < truncate (reconnectLimitValue * 1000))
                        then do
                          Com.dprintf $ NET.adrToString adr `B.append` ":reconnect rejected : too soon\n"
                          return True
                        else do
                          Com.printf $ NET.adrToString adr `B.append` ":reconnect\n"
                          gotNewClient (ClientReference idx) challenge userInfo adr qport
                          return True
                    else
                      findAndReuseIPSlot clients adr qport challenge userInfo (idx + 1) maxIdx

-- Initializes player structures after successful connection.
gotNewClient :: ClientReference -> Int -> B.ByteString -> NetAdrT -> Int -> Quake ()
gotNewClient clientRef@(ClientReference clientIdx) challenge userInfo adr qport = do
    -- build a new connection
    -- accept the new client
    -- this is the only place a client_t is ever initialized
    svGlobals.svClient .= Just clientRef

    let edictIdx = clientIdx + 1

    svGlobals.svServerStatic.ssClients.ix clientIdx.cEdict .= Just (newEdictReference edictIdx)

    -- save challenge for checksumming
    svGlobals.svServerStatic.ssClients.ix clientIdx.cChallenge .= challenge

    -- get the game a chance to reject this connection or modify the userinfo
    (allowed, userInfo') <- PlayerClient.clientConnect (newEdictReference edictIdx) userInfo

    if not allowed
      then do
        value <- Info.valueForKey userInfo' "rejmsg"

        if B.length value == 0
          then NetChannel.outOfBandPrint Constants.nsServer adr "print\nConnection refused.\n"
          else NetChannel.outOfBandPrint Constants.nsServer adr $ "print\n" `B.append` value `B.append` "\nConnection refused.\n"

        Com.dprintf "Game rejected a connection.\n"

      else do
        -- parse some info from the info strings
        svGlobals.svServerStatic.ssClients.ix clientIdx.cUserInfo .= userInfo'
        userInfoChanged clientRef

        -- send the connect packet to the client
        NetChannel.outOfBandPrint Constants.nsServer adr "client_connect"

        NetChannel.setup Constants.nsServer (svGlobals.svServerStatic.ssClients.ix clientIdx.cNetChan) adr qport

        svGlobals.svServerStatic.ssClients.ix clientIdx.cState .= Constants.csConnected

        Just buf <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx.cDatagramBuf
        SZ.init (svGlobals.svServerStatic.ssClients.ix clientIdx.cDatagram) buf Constants.maxMsgLen

        realTime <- use $ svGlobals.svServerStatic.ssRealTime

        zoom (svGlobals.svServerStatic.ssClients.ix clientIdx) $ do
          cDatagram.sbAllowOverflow .= True
          cLastMessage .= realTime
          cLastConnect .= realTime

        Com.dprintf "new client added.\n"

svcRemoteCommand :: Quake ()
svcRemoteCommand = io (putStrLn "SVMain.svcRemoteCommand") >> undefined -- TODO

{-
- Pull specific info from a newly changed userinfo string into a more C
- freindly form.
-}
userInfoChanged :: ClientReference -> Quake ()
userInfoChanged (ClientReference clientIdx) = do
    Just userInfo <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx.cUserInfo
    Just (Just edictRef) <- preuse $ svGlobals.svServerStatic.ssClients.ix clientIdx.cEdict

    -- call prog code to allow overrides
    void $ PlayerClient.clientUserInfoChanged edictRef userInfo

    -- name for C code
    name <- Info.valueForKey userInfo "name"
    svGlobals.svServerStatic.ssClients.ix clientIdx.cName .= name

    -- rate command
    val <- Info.valueForKey userInfo "rate"
    let rate = if B.length val > 0
                 then let i = Lib.atoi val
                      in if | i < 100 -> 100
                            | i > 15000 -> 15000
                            | otherwise -> i
                 else 5000

    svGlobals.svServerStatic.ssClients.ix clientIdx.cRate .= rate
    
    -- msg command
    msg <- Info.valueForKey userInfo "msg"
    when (B.length msg > 0) $
      svGlobals.svServerStatic.ssClients.ix clientIdx.cMessageLevel .= Lib.atoi msg
