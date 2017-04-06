{-# LANGUAGE FlexibleContexts #-}
module Server.SVMain
    ( dropClient
    , frame
    , initialize
    , shutdown
    ) where

import           Control.Applicative      (liftA2)
import           Control.Lens             (use, ix, (^.), (.=), (+=), (%=), (&), (.~))
import           Control.Monad            (void, when, unless, join, (>=>))
import           Data.Bits                ((.|.), (.&.))
import qualified Data.ByteString          as B
import qualified Data.Vector              as V
import qualified Data.Vector.Unboxed      as UV

import qualified Constants
import qualified Game.Cmd                 as Cmd
import           Game.CVarT
import           Game.EdictT
import           Game.EntityStateT
import qualified Game.GameBase            as GameBase
import           Game.GClientT
import qualified Game.Info                as Info
import qualified Game.PlayerClient        as PlayerClient
import           Game.PlayerStateT
import qualified QCommon.Com              as Com
import qualified QCommon.CVar             as CVar
import           QCommon.CVarVariables
import qualified QCommon.MSG              as MSG
import           QCommon.NetAdrT
import qualified QCommon.NetChannel       as NetChannel
import           QCommon.NetChanT
import           QCommon.SizeBufT
import qualified QCommon.SZ               as SZ
import           QuakeRef
import           QuakeState
import           Server.ChallengeT
import           Server.ClientT
import           Server.ServerStaticT
import           Server.ServerT
import qualified Server.SVConsoleCommands as SVConsoleCommands
import qualified Server.SVEnts            as SVEnts
import qualified Server.SVGame            as SVGame
import qualified Server.SVSend            as SVSend
import qualified Server.SVUser            as SVUser
import qualified Sys.NET                  as NET
import qualified Sys.Timer                as Timer
import           Types
import           Util.Binary              (encode)
import qualified Util.Lib                 as Lib

heartbeatSeconds :: Int
heartbeatSeconds = 300

remoteCommandHeader :: B.ByteString
remoteCommandHeader = B.pack [0xFF, 0xFF, 0xFF, 0xFF]

initialize :: Quake ()
initialize = do
    SVConsoleCommands.initOperatorCommands
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
frame msec = do
    globals.gTimeBeforeGame .= 0
    globals.gTimeAfterGame .= 0
    runFrame msec =<< use (svGlobals.svServerStatic.ssInitialized)

runFrame :: Int -> Bool -> Quake ()
runFrame _ False = return ()
runFrame msec True = do
    svGlobals.svServerStatic.ssRealTime += msec
    void Lib.rand
    checkTimeouts
    readPackets
    timeDemo <- timeDemoCVar
    realTime <- use (svGlobals.svServerStatic.ssRealTime)
    time <- use (svGlobals.svServer.sTime)
    checkTimeShift timeDemo realTime time

checkTimeShift :: CVarT -> Int -> Int -> Quake ()
checkTimeShift timeDemo realTime time
    | (timeDemo^.cvValue) == 0 && realTime < time = do
        when (time - realTime > 100) $
            rollBackRealTime =<< showClampCVar
        sleep =<< use (svGlobals.svServerStatic.ssRealTime)
    | otherwise = do
        calcPings
        giveMsec
        runGameFrame
        SVSend.sendClientMessages
        SVEnts.recordDemoMessage
        masterHeartbeat
        prepWorldFrame
  where
    rollBackRealTime showClamp = do
        when ((showClamp^.cvValue) /= 0) $
            Com.printf "sv lowclamp\n"
        svGlobals.svServerStatic.ssRealTime .= time - 100
    sleep updatedRealTime = NET.sleep (time - updatedRealTime)

checkTimeouts :: Quake ()
checkTimeouts = do
    realTime <- use (svGlobals.svServerStatic.ssRealTime)
    timeout <- timeoutCVar
    zombieTime <- zombieTimeCVar
    proceedCheckTimeouts realTime timeout zombieTime

proceedCheckTimeouts :: Int -> CVarT -> CVarT -> Quake ()
proceedCheckTimeouts realTime timeout zombieTime = do
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    mapM_ (checkClientTimeout realTime dropPoint zombiePoint . Ref) [0..maxClients-1]
  where
    dropPoint = realTime - truncate (1000 * (timeout^.cvValue))
    zombiePoint = realTime - truncate (1000 * (zombieTime^.cvValue))

checkClientTimeout :: Int -> Int -> Int -> Ref ClientT -> Quake ()
checkClientTimeout realTime dropPoint zombiePoint clientRef = do
    client <- readRef clientRef
    when ((client^.cLastMessage) > realTime) $
      modifyRef clientRef (\v -> v & cLastMessage .~ realTime)
    proceedCheckClientTimeout =<< readRef clientRef
  where
    proceedCheckClientTimeout client
        | (client^.cState) == Constants.csZombie && (client^.cLastMessage) < zombiePoint =
            modifyRef clientRef (\v -> v & cState .~ Constants.csFree)
        | ((client^.cState) == Constants.csConnected || (client^.cState) == Constants.csSpawned) && (client^.cLastMessage) < dropPoint = do
            SVSend.broadcastPrintf Constants.printHigh ((client^.cName) `B.append` " timed out\n")
            dropClient clientRef
            modifyRef clientRef (\v -> v & cState .~ Constants.csFree)
        | otherwise = return ()

readPackets :: Quake ()
readPackets = do
    gotSomething <- NET.getPacket Constants.nsServer (globals.gNetFrom) (globals.gNetMessage)
    when gotSomething checkPackets

checkPackets :: Quake ()
checkPackets = do
    header <- fmap (B.take 4) (use (globals.gNetMessage.sbData))
    choosePacket header
  where
    choosePacket header
        | header == remoteCommandHeader = do
            connectionlessPacket
            readPackets
        | otherwise = do
            MSG.beginReading (globals.gNetMessage)
            void (MSG.readLong (globals.gNetMessage)) -- sequence number
            void (MSG.readLong (globals.gNetMessage)) -- sequence number
            qport <- fmap (.&. 0xFFFF) (MSG.readShort (globals.gNetMessage))
            maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
            idx <- checkClientPackets (fromIntegral qport) 0 maxClients
            unless (idx >= maxClients) readPackets

checkClientPackets :: Int -> Int -> Int -> Quake Int
checkClientPackets qport idx maxIdx
    | idx >= maxIdx = return idx
    | otherwise = do
        client <- readRef (Ref idx)
        netAdr <- use (globals.gNetFrom)
        checkClientPacket client netAdr qport idx maxIdx

checkClientPacket :: ClientT -> NetAdrT -> Int -> Int -> Int -> Quake Int
checkClientPacket client netAdr qport idx maxIdx
    -- IMPROVE: first 3 can be merged into one
    | (client^.cState) == Constants.csFree = checkClientPackets qport (idx + 1) maxIdx
    | not (NET.compareBaseAdr netAdr (client^.cNetChan.ncRemoteAddress)) = checkClientPackets qport (idx + 1) maxIdx
    | (client^.cNetChan.ncRemoteQPort) /= qport = checkClientPackets qport (idx + 1) maxIdx
    | otherwise = do
        when ((client^.cNetChan.ncRemoteAddress.naPort) /= (netAdr^.naPort)) $ do
            Com.printf "SV_ReadPackets: fixing up a translated port\n"
            modifyRef clientRef (\v -> v & cNetChan.ncRemoteAddress.naPort .~ (netAdr^.naPort))
        ok <- NetChannel.process (svGlobals.svServerStatic.ssClients.ix idx.cNetChan) (globals.gNetMessage)
        when (ok &&(client^.cState) /= Constants.csZombie) $ do
            realTime <- use (svGlobals.svServerStatic.ssRealTime)
            modifyRef clientRef (\v -> v & cLastMessage .~ realTime)
            SVUser.executeClientMessage clientRef
        return idx
  where
    clientRef = Ref idx :: Ref ClientT

calcPings :: Quake ()
calcPings = do
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    mapM_ (readClient >=> updateClientPing) [0..maxClients-1]
  where
    readClient idx = do
        client <- readRef (Ref idx) :: Quake ClientT
        return (Ref idx, client)

updateClientPing :: (Ref ClientT, ClientT) -> Quake ()
updateClientPing (clientRef, client)
    | (client^.cState) == Constants.csSpawned = do
        modifyRef clientRef (\v -> v & cPing .~ ping)
        maybe edictRefError updateGClient (client^.cEdict)
    | otherwise = return ()
  where
    ping = calcFrameLatency client 0 0 0 Constants.latencyCounts
    edictRefError = Com.fatalError "SVMain.updateClientPing client^.cEdict is Nothing"
    updateGClient edictRef = do
        edict <- readRef edictRef
        maybe gClientRefError updateGClientPing (edict^.eClient)
    gClientRefError = Com.fatalError "SVMain.updateClientPing edict^.eClient is Nothing"
    updateGClientPing gClientRef =
        modifyRef gClientRef (\v -> v & gcPing .~ ping)

calcFrameLatency :: ClientT -> Int -> Int -> Int -> Int -> Int
calcFrameLatency client count total idx maxIdx
    | idx >= maxIdx && count == 0 = 0
    | idx >= maxIdx = total `div` count
    | (client^.cFrameLatency) UV.! idx > 0 =
        calcFrameLatency client (count + 1) (total + (client^.cFrameLatency) UV.! idx) (idx + 1) maxIdx
    | otherwise =
        calcFrameLatency client count total (idx + 1) maxIdx

giveMsec :: Quake ()
giveMsec = checkAndGive =<< use (svGlobals.svServer.sFrameNum)
  where
    checkAndGive frameNum
        | frameNum .&. 15 == 0 =
            svGlobals.svServerStatic.ssClients %= fmap giveTime
        | otherwise = return ()
    giveTime client
        | (client^.cState) == Constants.csFree = client
        | otherwise = client & cCommandMsec .~ 1800 -- 1600 + some slop

runGameFrame :: Quake ()
runGameFrame = do
    setTimeBeforeGame =<< hostSpeedsCVar
    svGlobals.svServer.sFrameNum += 1
    frameNum <- use (svGlobals.svServer.sFrameNum)
    svGlobals.svServer.sTime .= frameNum * 100
    join (liftA2 doRunGameFrame pausedCVar maxClientsCVar)
    setTimeAfterGame =<< hostSpeedsCVar
  where
    setTimeBeforeGame hostSpeeds
        | (hostSpeeds^.cvValue) /= 0 = do
            ms <- Timer.milliseconds
            globals.gTimeBeforeGame .= ms
        | otherwise = return ()
    setTimeAfterGame hostSpeeds
        | (hostSpeeds^.cvValue) /= 0 = do
            ms <- Timer.milliseconds
            globals.gTimeAfterGame .= ms
        | otherwise = return ()

doRunGameFrame :: CVarT -> CVarT -> Quake ()
doRunGameFrame paused maxClients
    | (paused^.cvValue) == 0 || (maxClients^.cvValue) > 1 = do
        GameBase.runFrame
        time <- use (svGlobals.svServer.sTime)
        realTime <- use (svGlobals.svServerStatic.ssRealTime)
        when (time < realTime) $ do
            showClamp <- showClampCVar
            when ((showClamp^.cvValue) /= 0) $
                Com.printf "sv highclamp\n"
            svGlobals.svServerStatic.ssRealTime .= time
    | otherwise = return ()

masterHeartbeat :: Quake ()
masterHeartbeat = do
    dedicated <- dedicatedCVar
    publicServer <- publicServerCVar
    sendMasterHeartbeat dedicated publicServer
  where
    sendMasterHeartbeat dedicated publicServer
        | (dedicated^.cvValue) /= 0 || (publicServer^.cvValue) == 0 = do
            lastHeartbeat <- use (svGlobals.svServerStatic.ssLastHeartbeat)
            realTime <- use (svGlobals.svServerStatic.ssRealTime)
            doSendHeartbeat lastHeartbeat realTime
        | otherwise = return ()

doSendHeartbeat :: Int -> Int -> Quake ()
doSendHeartbeat lastHeartbeat realTime
    | lastHeartbeat > realTime = svGlobals.svServerStatic.ssLastHeartbeat .= realTime
    | realTime - lastHeartbeat < heartbeatSeconds * 1000 = return ()
    | otherwise = do
        svGlobals.svServerStatic.ssLastHeartbeat .= realTime
        str <- statusString
        masterAdr <- use (svGlobals.svMasterAdr)
        mapM_ (sendToNetAdr str) (V.filter ((/= 0) . (^.naPort)) masterAdr)

sendToNetAdr :: B.ByteString -> NetAdrT -> Quake ()
sendToNetAdr msg netAdr = do
    Com.printf (B.concat ["Sending heartbeat to ", NET.adrToString netAdr, "\n"])
    NetChannel.outOfBandPrint Constants.nsServer netAdr ("heartbeat\n" `B.append` msg)

prepWorldFrame :: Quake ()
prepWorldFrame = do
    numEdicts <- use (gameBaseGlobals.gbNumEdicts)
    mapM_ resetEdictEvent [0..numEdicts-1]
  where
    resetEdictEvent idx =
        modifyRef (Ref idx) (\v -> v & eEntityState.esEvent .~ 0)

statusString :: Quake B.ByteString
statusString = do
    status <- fmap (`B.append` "\n") CVar.serverInfo
    maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
    clients <- use (svGlobals.svServerStatic.ssClients)
    V.foldM collectStatusString status (V.take maxClients clients)

collectStatusString :: B.ByteString -> ClientT -> Quake B.ByteString
collectStatusString acc client
    | clientInGame = do
        gClient <- getGClient
        return (appendClientInfo acc client gClient)
    | otherwise = return acc
  where
    clientInGame = (client^.cState) `elem` [Constants.csConnected, Constants.csSpawned]
    getGClient = do
        edictRef <- maybe edictError return (client^.cEdict)
        edict <- readRef edictRef
        readRef =<< maybe gClientError return (edict^.eClient)
    edictError = do
        Com.fatalError "SVMain.collectStatusString client^.cEdict is Nothing"
        return (Ref (-1))
    gClientError = do
        Com.fatalError "SVMain.collectStatusString edict^.eClient is Nothing"
        return (Ref (-1))

appendClientInfo :: B.ByteString -> ClientT -> GClientT -> B.ByteString
appendClientInfo acc client gClient
    | B.length acc + B.length player >= 1024 = acc
    | otherwise = acc `B.append` player
  where
    player = B.concat [ encode ((gClient^.gcPlayerState.psStats) UV.! Constants.statFrags)
                      , " ", encode (client^.cPing), "\"", client^.cName, "\"\n"
                      ]

connectionlessPacket :: Quake ()
connectionlessPacket = do
    MSG.beginReading (globals.gNetMessage)
    void (MSG.readLong (globals.gNetMessage)) -- skip the -1 marker
    str <- MSG.readStringLine (globals.gNetMessage)
    Cmd.tokenizeString str False
    command <- Cmd.argv 0
    from <- use (globals.gNetFrom)
    processConnectionlessPacket command from

processConnectionlessPacket :: B.ByteString -> NetAdrT -> Quake ()
processConnectionlessPacket "ping" from = svcPing from
processConnectionlessPacket "ack" from = svcAck from
processConnectionlessPacket "status" from = svcStatus from
processConnectionlessPacket "info" _ = svcInfo
processConnectionlessPacket "getchallenge" from = svcGetChallenge from
processConnectionlessPacket "connect" from = svcDirectConnect from
processConnectionlessPacket "rcon" _ = svcRemoteCommand
processConnectionlessPacket str from = do
    Com.printf (B.concat ["bad connectionless packet from ", NET.adrToString from, "\n"])
    Com.printf (B.concat ["[", str, "]\n"])
    -- IMPROVE: print hexdump of data like in jake2?

svcPing :: NetAdrT -> Quake ()
svcPing from = NetChannel.outOfBandPrint Constants.nsServer from "ack"

svcAck :: NetAdrT -> Quake ()
svcAck from = Com.printf (B.concat ["Ping acknowledge from ", NET.adrToString from, "\n"])

svcStatus :: NetAdrT -> Quake ()
svcStatus from = do
    status <- statusString
    NetChannel.outOfBandPrint Constants.nsServer from ("print\n" `B.append` status)

svcInfo :: Quake ()
svcInfo = error "SVMain.svcInfo" -- TODO

svcGetChallenge :: NetAdrT -> Quake ()
svcGetChallenge = error "SVMain.svcGetChallenge" -- TODO

svcDirectConnect :: NetAdrT -> Quake ()
svcDirectConnect adr = do
    Com.printf "SVC_DirectConnect ()\n"
    version <- fmap Lib.atoi (Cmd.argv 1)
    proceedDirectConnect version
  where
    proceedDirectConnect version
        | version /= Constants.protocolVersion = do
            NetChannel.outOfBandPrint Constants.nsServer adr (B.concat ["print\nServer is version ", encode Constants.version, "\n"]) -- IMPROVE ?
            Com.dprintf (B.concat ["    rejected connect from version ", encode version, "\n"])
        | otherwise = do
            qport <- fmap Lib.atoi (Cmd.argv 2)
            challenge <- fmap Lib.atoi (Cmd.argv 3)
            attractLoop <- use (svGlobals.svServer.sAttractLoop)
            userInfo <- getUserInfo
            doDirectConnect adr qport challenge attractLoop userInfo
    getUserInfo = do
        v <- Cmd.argv 4
        Info.setValueForKey v "ip" (NET.adrToString adr)

doDirectConnect :: NetAdrT -> Int -> Int -> Bool -> B.ByteString -> Quake ()
doDirectConnect adr qport challenge attractLoop userInfo
    | attractLoop && not isLocalAddress = do
        Com.printf "Remote connect in attract loop.  Ignored.\n"
        NetChannel.outOfBandPrint Constants.nsServer adr "print\nConnection refused.\n"
    | otherwise = do
        valid <- checkValidChallenge
        when valid $ do
            maxClients <- fmap (truncate . (^.cvValue)) maxClientsCVar
            clients <- fmap (V.take maxClients) (use (svGlobals.svServerStatic.ssClients))
            done <- findAndReuseIPSlot clients adr qport challenge userInfo 0 maxClients
            unless done $
                maybe serverFull foundEmptySlot (V.findIndex freeClient clients)
  where
    isLocalAddress = NET.isLocalAddress adr
    checkValidChallenge
        | not isLocalAddress = do
            challenges <- use (svGlobals.svServerStatic.ssChallenges)
            maybe noChallenge challengeFound (V.find challengeForAdr challenges)
        | otherwise = return True
    challengeForAdr c = NET.compareBaseAdr adr (c^.chAdr)
    noChallenge = do
        NetChannel.outOfBandPrint Constants.nsServer adr "print\nNo challenge for address.\n"
        return False
    challengeFound ch
        | (ch^.chChallenge) == challenge = return True
        | otherwise = do
            NetChannel.outOfBandPrint Constants.nsServer adr "print\nBad challenge.\n"
            return False
    freeClient c = c^.cState == Constants.csFree
    serverFull = do
        NetChannel.outOfBandPrint Constants.nsServer adr "print\nServer is full.\n"
        Com.dprintf "Rejected a connection.\n"
    foundEmptySlot idx = gotNewClient (Ref idx) challenge userInfo adr qport

findAndReuseIPSlot :: V.Vector ClientT -> NetAdrT -> Int -> Int -> B.ByteString -> Int -> Int -> Quake Bool
findAndReuseIPSlot clients adr qport challenge userInfo idx maxIdx
    | idx >= maxIdx = return False
    | otherwise = checkClientSlot (clients V.! idx)
  where
    checkClientSlot client
        | client^.cState == Constants.csFree =
            findAndReuseIPSlot clients adr qport challenge userInfo (idx + 1) maxIdx
        | NET.compareBaseAdr adr (client^.cNetChan.ncRemoteAddress) && ((client^.cNetChan.ncRemoteQPort) == qport || (adr^.naPort) == (client^.cNetChan.ncRemoteAddress.naPort)) = do
            realTime <- use (svGlobals.svServerStatic.ssRealTime)
            reconnectLimit <- svReconnectLimitCVar
            checkReconnect client realTime reconnectLimit
        | otherwise =
            findAndReuseIPSlot clients adr qport challenge userInfo (idx + 1) maxIdx
    checkReconnect client realTime reconnectLimit
        | not (NET.isLocalAddress adr) && (realTime - (client^.cLastConnect) < truncate ((reconnectLimit^.cvValue) * 1000)) = do
            Com.dprintf (NET.adrToString adr `B.append` ":reconnect rejected : too soon\n")
            return True
        | otherwise = do
            Com.printf (NET.adrToString adr `B.append` ":reconnect\n")
            gotNewClient (Ref idx) challenge userInfo adr qport
            return True

svcRemoteCommand :: Quake ()
svcRemoteCommand = error "SVMain.svcRemoteCommand" -- TODO

gotNewClient :: Ref ClientT -> Int -> B.ByteString -> NetAdrT -> Int -> Quake ()
gotNewClient clientRef@(Ref idx) challenge userInfo adr qport = do
    svGlobals.svClient .= Just clientRef
    modifyRef clientRef (\v -> v & cEdict .~ Just edictRef
                                 & cChallenge .~ challenge)
    (allowed, userInfo') <- PlayerClient.clientConnect edictRef userInfo
    checkConnectionAllowed clientRef adr qport allowed userInfo'
  where
    edictRef = Ref (idx + 1)

checkConnectionAllowed :: Ref ClientT -> NetAdrT -> Int -> Bool -> B.ByteString -> Quake ()
checkConnectionAllowed clientRef@(Ref idx) adr qport allowed userInfo
    | not allowed = do
        value <- Info.valueForKey userInfo "rejmsg"
        printRefusedMessage value
        Com.dprintf "Game rejected a connection.\n"
    | otherwise = do
        modifyRef clientRef (\v -> v & cUserInfo .~ userInfo)
        userInfoChanged clientRef
        NetChannel.outOfBandPrint Constants.nsServer adr "client_connect"
        NetChannel.setup Constants.nsServer (svGlobals.svServerStatic.ssClients.ix idx.cNetChan) adr qport
        modifyRef clientRef (\v -> v & cState .~ Constants.csConnected)
        initAndAddClient clientRef
  where
    printRefusedMessage str
        | B.null str = NetChannel.outOfBandPrint Constants.nsServer adr "print\nConnection refused.\n"
        | otherwise = NetChannel.outOfBandPrint Constants.nsServer adr (B.concat ["print\n", str, "\nConnection refused.\n"])

initAndAddClient :: Ref ClientT -> Quake ()
initAndAddClient clientRef@(Ref idx) = do
    SZ.initialize (svGlobals.svServerStatic.ssClients.ix idx.cDatagram) B.empty Constants.maxMsgLen
    realTime <- use (svGlobals.svServerStatic.ssRealTime)
    modifyRef clientRef (\v -> v & cDatagram.sbAllowOverflow .~ True
                                 & cLastMessage .~ realTime
                                 & cLastConnect .~ realTime)
    Com.dprintf "new client added.\n"

userInfoChanged :: Ref ClientT -> Quake ()
userInfoChanged clientRef = do
    client <- readRef clientRef
    maybe edictRefError (proceedUserInfoChanged client) (client^.cEdict)
  where
    edictRefError = Com.fatalError "SVMain.userInfoChanged client^.cEdict is Nothing"
    proceedUserInfoChanged client edictRef = do
        void (PlayerClient.clientUserInfoChanged edictRef (client^.cUserInfo))
        name <- Info.valueForKey (client^.cUserInfo) "name"
        modifyRef clientRef (\v -> v & cName .~ name)
        setRateCommand =<< Info.valueForKey (client^.cUserInfo) "rate"
        msg <- Info.valueForKey (client^.cUserInfo) "msg"
        when (B.length msg > 0) $
            modifyRef clientRef (\v -> v & cMessageLevel .~ Lib.atoi msg)
    setRateCommand val =
        modifyRef clientRef (\v -> v & cRate .~ calcRate val)
    calcRate val
        | B.length val > 0 = convertValToRate (Lib.atoi val)
        | otherwise = 5000
    convertValToRate val
        | val < 100 = 100
        | val > 15000 = 15000
        | otherwise = val

shutdown :: B.ByteString -> Bool -> Quake ()
shutdown finalMsg reconnect = do
    clients <- use (svGlobals.svServerStatic.ssClients)
    when (V.null clients) $ 
        finalMessage finalMsg reconnect
    masterShutdown
    SVGame.shutdownGameProgs
    -- free current level
    sDemoFileHandle <- use (svGlobals.svServer.sDemoFile)
    maybe (return ()) Lib.fClose sDemoFileHandle
    svGlobals.svServer .= newServer
    globals.gServerState .= (newServer^.sState)
    ssDemoFileHandle <- use (svGlobals.svServerStatic.ssDemoFile)
    maybe (return ()) Lib.fClose ssDemoFileHandle
    svGlobals.svServerStatic .= newServerStaticT
  where
    newServer = newServerT

finalMessage :: B.ByteString -> Bool -> Quake ()
finalMessage = error "SVMainShared.finalMessage" -- TODO

masterShutdown :: Quake ()
masterShutdown = do
    dedicated <- fmap (^.cvValue) dedicatedCVar
    publicServer <- fmap (^.cvValue) publicServerCVar
    unless (dedicated == 0 || publicServer == 0) $ do
        error "SVMainShared.masterShutdown" -- TODO

dropClient :: Ref ClientT -> Quake ()
dropClient = error "SVMain.dropClient" -- TODO
