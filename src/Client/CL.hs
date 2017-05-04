module Client.CL
    ( clearState
    , dropClient
    , fixUpGender
    , frame
    , initialize
    , pingServersF
    , quitF
    , shutdown
    , writeConfiguration
    , writeDemoMessage
    ) where

import           Control.Concurrent       (threadDelay)
import           Control.Lens             (preuse, use, ix)
import           Control.Lens             ((^.), (.=), (%=), (+=), (&), (.~))
import           Control.Monad            (unless, when, void)
import           Data.Binary.Get          (runGet)
import           Data.Bits                (xor, (.&.))
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe               (fromMaybe)
import qualified Data.Vector              as Vector
import           Linear                   (V4(..))
import           System.IO                (Handle, IOMode(..), SeekMode(..))
import           System.IO                (hSeek, hSetFileSize)

import           Client.CEntityT
import           Client.CheatVarT
import qualified Client.CLFX              as CLFX
import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.CLInput           as CLInput
import qualified Client.CLParse           as CLParse
import qualified Client.CLPred            as CLPred
import qualified Client.CLTEnt            as CLTEnt
import qualified Client.CLView            as CLView
import qualified Client.Console           as Console
import qualified Client.Key               as Key
import qualified Client.Menu              as Menu
import           Client.RefDefT
import           Client.RefExportT
import qualified Client.SCR               as SCR
import qualified Client.V                 as V
import qualified Client.VID               as VID
import qualified Constants
import           Data.Bits                ((.|.))
import qualified Game.Cmd                 as Cmd
import           Game.CVarT
import qualified Game.Info                as Info
import           Game.MapSurfaceT
import qualified QCommon.CBuf             as CBuf
import qualified QCommon.CM               as CM
import qualified QCommon.Com              as Com
import qualified QCommon.CVar             as CVar
import           QCommon.CVarVariables
import qualified QCommon.FS               as FS
import qualified QCommon.MSG              as MSG
import           QCommon.NetAdrT
import qualified QCommon.NetChannel       as NetChannel
import           QCommon.NetChanT
import           QCommon.QFiles.MD2.DMdlT
import           QCommon.SizeBufT
import qualified QCommon.SZ               as SZ
import           QCommon.XCommandT        (runXCommandT)
import           QuakeState
import           Render.Renderer
import qualified Sound.S                  as S
import qualified Sys.IN                   as IN
import qualified Sys.NET                  as NET
import qualified Sys.Sys                  as Sys
import qualified Sys.Timer                as Timer
import           Types
import           Util.Binary              (encode)
import qualified Util.Lib                 as Lib

playerMult :: Int
playerMult = 5

-- ENV_CNT is map load, ENV_CNT+1 is first env map
envCnt :: Int
envCnt = Constants.csPlayerSkins + Constants.maxClients * playerMult

textureCnt :: Int
textureCnt = envCnt + 13

envSuf :: Vector.Vector B.ByteString
envSuf = Vector.fromList [ "rt", "bk", "lf", "ft", "up", "dn" ]

remoteCommandHeader :: B.ByteString
remoteCommandHeader = B.pack [0xFF, 0xFF, 0xFF, 0xFF]

cheatVars :: Vector.Vector CheatVarT
cheatVars = Vector.fromList
    [ CheatVarT "timescale" "1"          , CheatVarT "timedemo" "0"
    , CheatVarT "r_drawworld" "1"        , CheatVarT "cl_testlights" "0"
    , CheatVarT "r_fullbright" "0"       , CheatVarT "r_drawflat" "0"
    , CheatVarT "paused" "0"             , CheatVarT "fixedtime" "0"
    , CheatVarT "sw_draworder" "0"       , CheatVarT "gl_lightmap" "0"
    , CheatVarT "gl_saturatelighting" "0"
    ]

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars =
    [ ("adr0", B.empty, Constants.cvarArchive)
    , ("adr1", B.empty, Constants.cvarArchive)
    , ("adr2", B.empty, Constants.cvarArchive)
    , ("adr3", B.empty, Constants.cvarArchive)
    , ("adr4", B.empty, Constants.cvarArchive)
    , ("adr5", B.empty, Constants.cvarArchive)
    , ("adr6", B.empty, Constants.cvarArchive)
    , ("adr7", B.empty, Constants.cvarArchive)
    , ("adr8", B.empty, Constants.cvarArchive)
    , ("cl_stereo_separation", "0.4", Constants.cvarArchive)
    , ("cl_stereo", "0", 0), ("cl_blend", "1", 0), ("cl_lights", "1", 0)
    , ("cl_particles", "1", 0), ("cl_entities", "1", 0), ("cl_gun", "1", 0)
    , ("cl_footsteps", "1", 0), ("cl_noskins", "0", 0), ("cl_autoskins", "0", 0)
    , ("cl_predict", "1", 0), ("cl_maxfps", "90", 0), ("cl_upspeed", "200", 0)
    , ("cl_forwardspeed", "200", 0), ("cl_sidespeed", "200", 0)
    , ("cl_yawspeed", "140", 0), ("cl_pitchspeed", "150", 0)
    , ("cl_anglespeedkey", "1.5", 0), ("cl_run", "0", Constants.cvarArchive)
    , ("lookspring", "0", Constants.cvarArchive)
    , ("lookstrafe", "0", Constants.cvarArchive)
    , ("sensitivity", "3", Constants.cvarArchive)
    , ("m_pitch", "0.022", Constants.cvarArchive)
    , ("m_yaw", "0.022", 0), ("m_forward", "1", 0), ("m_side", "1", 0)
    , ("cl_shownet", "0", 0), ("cl_showmiss", "0", 0), ("showclamp", "0", 0)
    , ("cl_timeout", "120", 0), ("paused", "0", 0), ("timedemo", "0", 0)
    , ("rcon_password", B.empty, 0), ("rcon_address", B.empty, 0)
    , ("r_lightlevel", "0", 0), ("password", B.empty, Constants.cvarUserInfo)
    , ("spectator", "0", Constants.cvarUserInfo)
    , ("name", "unnamed", Constants.cvarUserInfo .|. Constants.cvarArchive)
    , ("skin", "male/grunt", Constants.cvarUserInfo .|. Constants.cvarArchive)
    , ("rate", "25000", Constants.cvarUserInfo .|. Constants.cvarArchive) -- FIXME
    , ("msg", "1", Constants.cvarUserInfo .|. Constants.cvarArchive)
    , ("hand", "0", Constants.cvarUserInfo .|. Constants.cvarArchive)
    , ("fov", "90", Constants.cvarUserInfo .|. Constants.cvarArchive)
    , ("gender", "male", Constants.cvarUserInfo .|. Constants.cvarArchive)
    , ("gender_auto", "1", Constants.cvarArchive)
    , ("cl_vwep", "1", Constants.cvarArchive)
    ]

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands =
    [ ("cmd", Just forwardToServerF), ("pause", Just pauseF)
    , ("pingservers", Just pingServersF), ("skins", Just skinsF)
    , ("userinfo", Just userInfoF), ("snd_restart", Just sndRestartF)
    , ("changing", Just changingF), ("disconnect", Just disconnectF)
    , ("record", Just recordF), ("stop", Just stopF) , ("quit", Just quitF)
    , ("connect", Just connectF), ("reconnect", Just reconnectF)
    , ("rcon", Just rconF), ("precache", Just precacheF)
    , ("download", Just CLParse.downloadF), ("wave", Nothing)
    , ("inven", Nothing) , ("kill", Nothing), ("use", Nothing)
    , ("drop", Nothing) , ("say", Nothing), ("say_team", Nothing)
    , ("info", Nothing) , ("prog", Nothing), ("give", Nothing)
    , ("god", Nothing) , ("notarget", Nothing), ("noclip", Nothing)
    , ("invuse", Nothing) , ("invprev", Nothing), ("invnext", Nothing)
    , ("invdrop", Nothing) , ("weapnext", Nothing), ("weapprev", Nothing)
    ]

initialize :: Quake ()
initialize = do
    inDedicatedMode <- fmap ((/= 0) . (^.cvValue)) dedicatedCVar
    unless inDedicatedMode $ do
        Console.initialize
        S.initialize
        VID.initialize
        V.initialize
        SZ.initialize (globals.gNetMessage) B.empty Constants.maxMsgLen
        Menu.initialize
        SCR.initialize
        initializeLocal
        IN.initialize
        FS.execAutoexec
        CBuf.execute

writeConfiguration :: Quake ()
writeConfiguration = do
    path <- getConfigurationFilePath
    fileHandle <- Lib.fOpen path ReadWriteMode
    maybe fileHandleError (proceedWriteConfiguration path) fileHandle
  where 
    fileHandleError = error "CL.writeConfiguration fileHandle is Nothing"
    proceedWriteConfiguration path fileHandle = do
        io (writeConfigFileHeader fileHandle)
        Key.writeBindings fileHandle
        Lib.fClose fileHandle
        CVar.writeVariables path

getConfigurationFilePath :: Quake B.ByteString
getConfigurationFilePath = do
    gameDir <- FS.gameDir
    return (gameDir `B.append` "/config.cfg")

-- IMPROVE: exceptions
writeConfigFileHeader :: Handle -> IO ()
writeConfigFileHeader fileHandle = do
    hSeek fileHandle AbsoluteSeek 0
    hSetFileSize fileHandle 0
    B.hPut fileHandle "// generated by quake, do not modify\n"

initializeLocal :: Quake ()
initializeLocal = do
    initializeClientState
    CLInput.initializeInput
    CVar.initializeCVars initialCVars
    clearGenderCVar
    Cmd.addInitialCommands initialCommands
  where
    initializeClientState = do
        globals.gCls.csState .= Constants.caDisconnected
        msec <- Timer.milliseconds
        globals.gCls.csRealTime .= msec
    clearGenderCVar = do
        gender <- genderCVar
        CVar.update (gender & cvModified .~ False)

forwardToServerF :: XCommandT
forwardToServerF = XCommandT "CL.forwardToServerF" forwardToServer

forwardToServer :: Quake ()
forwardToServer = checkState =<< use (globals.gCls.csState)
  where
    checkState state
        | state `notElem` [Constants.caConnected, Constants.caActive] = do
            arg <- Com.argv 0
            Com.printf (B.concat ["Can't \"", arg, "\", not connected\n"])
        | otherwise = do
            c <- Cmd.argc
            when (c > 1) $ do
                MSG.writeByteI (globals.gCls.csNetChan.ncMessage) Constants.clcStringCmd
                SZ.printSB (globals.gCls.csNetChan.ncMessage) =<< Cmd.args

pauseF :: XCommandT
pauseF = XCommandT "CL.pauseF" $ do
    maxClients <- CVar.variableValue "maxclients"
    state <- use (globals.gServerState)
    doPause maxClients state
  where
    doPause maxClients state
        | maxClients > 1 || state == 0 = CVar.setValueI "paused" 0
        | otherwise = do
            paused <- pausedCVar
            CVar.setValueF "paused" (paused^.cvValue)

pingServersF :: XCommandT
pingServersF = error "CL.pingServersF" -- TODO

skinsF :: XCommandT
skinsF = error "CL.skinsF" -- TODO

userInfoF :: XCommandT
userInfoF = XCommandT "CL.userInfoF" $ do
    Com.printf "User info settings:\n"
    Info.printInfo =<< CVar.userInfo

sndRestartF :: XCommandT
sndRestartF = XCommandT "CL.sndRestartF" $ do
    S.shutdown
    S.initialize
    CLParse.registerSounds

changingF :: XCommandT
changingF = XCommandT "CL.changingF" $
    doChanging =<< use (globals.gCls.csDownload)
  where
    doChanging (Just _) = return ()
    doChanging Nothing =
      do SCR.beginLoadingPlaque
         globals.gCls.csState .= Constants.caConnected
         Com.printf "\nChanging map...\n"

disconnectF :: XCommandT
disconnectF = XCommandT "CL.disconnectF" $
    Com.comError Constants.errDrop "Disconnected from server"

recordF :: XCommandT
recordF = error "CL.recordF" -- TODO

stopF :: XCommandT
stopF = error "CL.stopF" -- TODO

connectF :: XCommandT
connectF = error "CL.connectF" -- TODO

reconnectF :: XCommandT
reconnectF = XCommandT "CL.reconnectF" $ do
    -- ZOID
    -- if we are downloading, we dont' change! This so we don't suddenly
    -- stop downloading a map
    cls <- use (globals.gCls)
    maybe (reconnect cls) (\_ -> return ()) (cls^.csDownload)
  where
    reconnect cls = do
        S.stopAllSounds
        doReconnect cls
    doReconnect cls
        | (cls^.csState) == Constants.caConnected = do
            Com.printf "reconnecting...\n"
            globals.gCls.csState .= Constants.caConnected
            MSG.writeCharI (globals.gCls.csNetChan.ncMessage) Constants.clcStringCmd
            MSG.writeString (globals.gCls.csNetChan.ncMessage) "new"
        | not (B.null (cls^.csServerName)) = do
            setConnectTime cls
            globals.gCls.csState .= Constants.caConnecting
            Com.printf "reconnecting...\n"
        | otherwise = return ()
    setConnectTime cls
        | (cls^.csState) >= Constants.caConnected = do
            disconnect
            globals.gCls.csConnectTime .= fromIntegral (cls^.csRealTime) - 1500
        | otherwise =
            globals.gCls.csConnectTime .= (-99999) -- fire immediately

rconF :: XCommandT
rconF = error "CL.rconF" -- TODO

precacheF :: XCommandT
precacheF = XCommandT "CL.precacheF" $
    precache =<< Cmd.argc
  where
    precache c
        | c < 2 =
            do name <- preuse (globals.gCl.csConfigStrings.ix (Constants.csModels + 1))
               maybe nameError loadMapAndRefresh name
        | otherwise =
            do arg <- Cmd.argv 1
               clientGlobals %= (\v -> v & cgPrecacheCheck .~ Constants.csModels
                                         & cgPrecacheSpawnCount .~ Lib.atoi arg
                                         & cgPrecacheModel .~ Nothing
                                         & cgPrecacheModelSkin .~ 0)
               requestNextDownload
    nameError = Com.fatalError "CL.precacheF name is Nothing"
    loadMapAndRefresh name = do
        void (CM.loadMap name True [0])
        CLParse.registerSounds
        CLView.prepRefresh

frame :: Int -> Quake ()
frame msec = runFrame msec =<< dedicatedCVar

runFrame :: Int -> CVarT -> Quake ()
runFrame msec dedicated
    | (dedicated^.cvValue) /= 0 = return ()
    | otherwise = do
        clientGlobals.cgExtraTime += msec
        skip <- shouldSkip
        unless skip (clientFrame msec)

shouldSkip :: Quake Bool
shouldSkip = do
    timeDemo <- timeDemoCVar
    maxFps <- clMaxFPSCVar
    state <- use (globals.gCls.csState)
    extraTime <- use (clientGlobals.cgExtraTime)
    checkSkipConditions timeDemo maxFps state extraTime
  where
    checkSkipConditions timeDemo maxFps state extraTime
        | (timeDemo^.cvValue) == 0 &&
          ((state == Constants.caConnected && extraTime < 100) ||
           (fromIntegral extraTime < 1000 / (maxFps^.cvValue))) =
            return True
        | otherwise = return False

clientFrame :: Int -> Quake ()
clientFrame msec = do
    IN.frame
    calcSimulationTime
    checkDebugger msec
    readPackets
    sendCommand
    CLPred.predictMovement
    renderingDLLChange
    SCR.updateScreen
    updateAudio
    CLFX.runDLights
    CLFX.runLightStyles
    SCR.runCinematic
    SCR.runConsole
    globals.gCls.csFrameCount += 1
    checkKeyDest

calcSimulationTime :: Quake ()
calcSimulationTime = do
    extraTime <- use (clientGlobals.cgExtraTime)
    curTime <- Timer.getCurTime
    globals.gCls.csFrameTime .= max (fromIntegral extraTime / 1000) 0.2
    globals.gCls.csRealTime .= curTime
    globals.gCl.csTime += extraTime
    clientGlobals.cgExtraTime .= 0

checkDebugger :: Int -> Quake ()
checkDebugger msec
    | msec > 5000 = do
        millis <- Timer.milliseconds
        globals.gCls.csNetChan.ncLastReceived .= millis
    | otherwise = return ()

renderingDLLChange :: Quake ()
renderingDLLChange = do
    VID.checkChanges
    state <- use (globals.gCls.csState)
    refreshPrepped <- use (globals.gCl.csRefreshPrepped)
    when (not refreshPrepped && state == Constants.caActive) $
        CLView.prepRefresh
      {- TODO: do we need this?
         cinematicTime <- use (globals.gCl.csCinematicTime)
         when (cinematicTime == 0) $
           request (io performGC)
           -}
            
updateAudio :: Quake ()
updateAudio = do
    cl <- use (globals.gCl)
    S.update (cl^.csRefDef.rdViewOrg) (cl^.csVForward) (cl^.csVRight) (cl^.csVUp)

checkKeyDest :: Quake ()
checkKeyDest = do
    keyDest <- use (globals.gCls.csKeyDest)
    state <- use (globals.gCls.csState)
    when (state /= Constants.caActive || keyDest /= Constants.keyGame) $
        io (threadDelay (20 * 1000))

dropClient :: Quake ()
dropClient = do
    clientStatic <- use (globals.gCls)
    when ((clientStatic^.csState) `notElem` [Constants.caUninitialized, Constants.caDisconnected])
        disconnect
    when ((clientStatic^.csDisableServerCount) /= -1)
        SCR.endLoadingPlaque

sendCommand :: Quake ()
sendCommand = do
    Sys.sendKeyEvents
    IN.commands
    CBuf.execute
    fixCVarCheats
    CLInput.sendCmd
    checkForResend

readPackets :: Quake ()
readPackets = do
    nextPacket
    checkTimeout

checkTimeout :: Quake ()
checkTimeout = do
    cls <- use (globals.gCls)
    timeout <- timeoutCVar
    doCheckTimeout cls timeout
  where
    doCheckTimeout cls timeout
        | (cls^.csState) >= Constants.caConnected && (cls^.csRealTime) - (cls^.csNetChan.ncLastReceived) > (truncate (timeout^.cvValue)) * 1000 = do
            globals.gCl.csTimeOutCount += 1
            serverTimeout =<< use (globals.gCl.csTimeOutCount)
        | otherwise = globals.gCl.csTimeOutCount .= 0
    serverTimeout count
        | count > 5 = do
            Com.printf "\nServer connection timed out.\n"
            disconnect
        | otherwise = return ()

nextPacket :: Quake ()
nextPacket = do
    gotSomething <- NET.getPacket Constants.nsClient (globals.gNetFrom) (globals.gNetMessage)
    when gotSomething $ do
        cls <- use (globals.gCls)
        netMsg <- use (globals.gNetMessage)
        processPacket cls netMsg
        nextPacket

processPacket :: ClientStaticT -> SizeBufT -> Quake ()
processPacket cls netMsg
    | B.take 4 (netMsg^.sbData) == remoteCommandHeader = connectionlessPacket
    | (cls^.csState) `elem` [Constants.caDisconnected, Constants.caConnecting] = return ()
    | (netMsg^.sbCurSize) < 8 = do
        from <- use (globals.gNetFrom)
        Com.printf ((NET.adrToString from) `B.append` ": Runt packet\n")
    | otherwise = do
        from <- use (globals.gNetFrom)
        remote <- use (globals.gCls.csNetChan.ncRemoteAddress)
        checkSame from remote
  where
    checkSame from remote
        | not (NET.compareAdr from remote) =
            Com.dprintf ((NET.adrToString from) `B.append` ":sequenced packet without connection\n")
        | otherwise = do
            ok <- NetChannel.process (globals.gCls.csNetChan) (globals.gNetMessage)
            when ok CLParse.parseServerMessage

connectionlessPacket :: Quake ()
connectionlessPacket = do
    MSG.beginReading (globals.gNetMessage)
    void (MSG.readLong (globals.gNetMessage)) -- skip the -1 marker
    str <- MSG.readStringLine (globals.gNetMessage)
    Cmd.tokenizeString str False
    command <- Cmd.argv 0
    from <- use (globals.gNetFrom)
    Com.printf (B.concat [NET.adrToString from, ": ", command, "\n"])
    processConnectionlessPacket command from

processConnectionlessPacket :: B.ByteString -> NetAdrT -> Quake ()
processConnectionlessPacket "client_connect" from = clcClientConnect from
processConnectionlessPacket "info"           from = parseStatusMessage from
processConnectionlessPacket "cmd"            from = clcCmd from
processConnectionlessPacket "print"          _    = clcPrint
processConnectionlessPacket "ping"           from = clcPing from
processConnectionlessPacket "challenge"      _    = clcChallenge
processConnectionlessPacket "echo"           from = clcEcho from
processConnectionlessPacket _                _    = Com.printf "Unknown command.\n"

clcClientConnect :: NetAdrT -> Quake ()
clcClientConnect from =
    doClientConnect =<< use (globals.gCls.csState)
  where
    doClientConnect state
        | state == Constants.caConnected =
            Com.printf "Dup connect received.  Ignored.\n"
        | otherwise = do
            qport <- use (globals.gCls.csQuakePort)
            NetChannel.setup Constants.nsClient (globals.gCls.csNetChan) from qport
            MSG.writeCharI (globals.gCls.csNetChan.ncMessage) (fromIntegral Constants.clcStringCmd)
            MSG.writeString (globals.gCls.csNetChan.ncMessage) "new"
            globals.gCls.csState .= Constants.caConnected

parseStatusMessage :: NetAdrT -> Quake ()
parseStatusMessage from = do
    str <- MSG.readString (globals.gNetMessage)
    Com.printf (str `B.append` "\n")
    Menu.addToServerList from str

clcCmd :: NetAdrT -> Quake ()
clcCmd from
    | not (NET.isLocalAddress from) =
        Com.printf "Command packet from remote host.  Ignored.\n"
    | otherwise = do
        cmd <- MSG.readString (globals.gNetMessage)
        CBuf.addText cmd
        CBuf.addText "\n"

clcPrint :: Quake ()
clcPrint = do
    msg <- MSG.readString (globals.gNetMessage)
    when (B.length msg > 0) (Com.printf msg)

clcPing :: NetAdrT -> Quake ()
clcPing from = NetChannel.outOfBandPrint Constants.nsClient from "ack"

clcChallenge :: Quake ()
clcChallenge = do
    v1 <- Cmd.argv 1
    globals.gCls.csChallenge .= Lib.atoi v1
    sendConnectPacket

clcEcho :: NetAdrT -> Quake ()
clcEcho from = do
    v1 <- Cmd.argv 1
    NetChannel.outOfBandPrint Constants.nsClient from v1

fixCVarCheats :: Quake ()
fixCVarCheats = do
    maxClients <- preuse (globals.gCl.csConfigStrings.ix Constants.csMaxClients)
    maybe maxClientsError checkSingleOrMultiplayer maxClients
  where
    maxClientsError = error "CL.fixCVarCheats maxClients is Nothing"
    checkSingleOrMultiplayer maxClients
        | maxClients == "1" || B.null maxClients = return () -- single player can cheat
        | otherwise = Vector.mapM_ fixCVar cheatVars

fixCVar :: CheatVarT -> Quake ()
fixCVar cheatVar = do
    var <- getExisting (cheatVar^.chvName) -- from CVarVariables
    when ((var^.cvString) /= (cheatVar^.chvValue)) $
        void (CVar.set (cheatVar^.chvName) (cheatVar^.chvValue))

checkForResend :: Quake ()
checkForResend = do
    state <- use (globals.gCls.csState)
    serverState <- use (globals.gServerState)
    doCheckForResend state serverState
  where
    doCheckForResend state serverState
        | state == Constants.caDisconnected && serverState /= 0 = do
            globals.gCls.csState .= Constants.caConnecting
            globals.gCls.csServerName .= "localhost"
            sendConnectPacket
        | otherwise = do
            realTime <- use (globals.gCls.csRealTime)
            connectTime <- use (globals.gCls.csConnectTime)
            unless (state /= Constants.caConnecting || (fromIntegral realTime - connectTime) < 3000) $ do
                serverName <- use (globals.gCls.csServerName)
                adrMaybe <- NET.stringToAdr serverName
                maybe badServerError (resend realTime serverName) adrMaybe
    badServerError = do
        Com.printf "Bad server address\n"
        globals.gCls.csState .= Constants.caDisconnected

resend :: Int -> B.ByteString -> NetAdrT -> Quake ()
resend realTime serverName adr = do
    globals.gCls.csConnectTime .= fromIntegral realTime
    Com.printf (B.concat ["Connecting to ", serverName, "...\n"])
    NetChannel.outOfBandPrint Constants.nsClient address "getchallenge\n"
  where
    address | (adr^.naPort) == 0 = adr & naPort .~ Constants.portServer
            | otherwise = adr

sendConnectPacket :: Quake ()
sendConnectPacket = do
    serverName <- use (globals.gCls.csServerName)
    adr <- NET.stringToAdr serverName
    maybe badServerError doSendConnectPacket adr
  where
    badServerError = do
        Com.printf "Bad server address\n"
        globals.gCls.csConnectTime .= 0

doSendConnectPacket :: NetAdrT -> Quake ()
doSendConnectPacket adr = do
    port <- fmap truncate (CVar.variableValue "qport") :: Quake Int
    challenge <- use (globals.gCls.csChallenge)
    userInfo <- CVar.userInfo
    globals.gUserInfoModified .= False
    NetChannel.outOfBandPrint Constants.nsClient address (connectString port challenge userInfo)
  where
    address | (adr^.naPort) == 0 = adr & naPort .~ Constants.portServer
            | otherwise = adr
    connectString port challenge userInfo =
        B.concat [ "connect ", encode Constants.protocolVersion, " "
                 , encode port, " ", encode challenge, " \"", userInfo, "\"\n"]

shutdown :: Quake ()
shutdown = doShutdown =<< use (clientGlobals.cgIsDown)
  where
    doShutdown True = io (putStrLn "recursive shutdown\n")
    doShutdown False = do
        clientGlobals.cgIsDown .= True
        writeConfiguration
        S.shutdown
        IN.shutdown
        VID.shutdown

quitF :: XCommandT
quitF = XCommandT "CL.quitF" (disconnect >> Com.quit)

fixUpGender :: Quake ()
fixUpGender = error "CL.fixUpGender" -- TODO

disconnect :: Quake ()
disconnect = do
    cl <- use (globals.gCl)
    cls <- use (globals.gCls)
    unless ((cls^.csState) == Constants.caDisconnected) $ do
        timeDemo <- fmap (^.cvValue) timeDemoCVar
        when (timeDemo /= 0) $ do
            ms <- Timer.milliseconds
            let time = fromIntegral (ms - (cl^.csTimeDemoStart)) :: Float
            when (time > 0) $
                Com.printf (B.concat [encode (cl^.csTimeDemoFrames), " frames, ", encode (time / 1000.0), " seconds: ", encode (fromIntegral (cl^.csTimeDemoFrames) * 1000.0 / time), " fps\n"])
        globals.gCl.csRefDef.rdBlend .= V4 0 0 0 0
        renderer <- use (globals.gRenderer)
        (renderer^.rRefExport.reCinematicSetPalette) Nothing
        Menu.forceMenuOff
        globals.gCls.csConnectTime .= 0
        SCR.stopCinematic
        when (cls^.csDemoRecording) $
            runXCommandT stopF
        -- send a disconnect message to the server
        let fin = B.pack [fromIntegral Constants.clcStringCmd] `B.append` "disconnect"
        NetChannel.transmit (globals.gCls.csNetChan) (B.length fin) fin
        NetChannel.transmit (globals.gCls.csNetChan) (B.length fin) fin
        NetChannel.transmit (globals.gCls.csNetChan) (B.length fin) fin
        clearState
        -- stop download
        maybe (return ()) stopDownload (cls^.csDownload)
        globals.gCls.csState .= Constants.caDisconnected
  where
    stopDownload handle = do
        Lib.fClose handle
        globals.gCls.csDownload .= Nothing


writeDemoMessage :: Quake ()
writeDemoMessage = error "CL.writeDemoMessage" -- TODO

clearState :: Quake ()
clearState = do
    S.stopAllSounds
    CLFX.clearEffects
    CLTEnt.clearTEnts
    globals.gCl .= newClientStateT
    globals.gClEntities .= Vector.replicate Constants.maxEdicts newCEntityT
    SZ.clear (globals.gCls.csNetChan.ncMessage)

requestNextDownload :: Quake ()
requestNextDownload = do
    state <- use (globals.gCls.csState)
    when (state == Constants.caConnected) $ do
        allowDownload <- fmap (^.cvValue) allowDownloadCVar
        precacheCheck <- use (clientGlobals.cgPrecacheCheck)
        when (allowDownload == 0 && precacheCheck < envCnt) $
            clientGlobals.cgPrecacheCheck .= envCnt
        startWithMap =<< use (clientGlobals.cgPrecacheCheck)
  where
    startWithMap precacheCheck
        | precacheCheck == Constants.csModels = do
            clientGlobals.cgPrecacheCheck += 2
            allowDownloadMaps <- fmap (^.cvValue) allowDownloadMapsCVar
            downloadMap allowDownloadMaps
        | otherwise = proceedWithModels =<< use (clientGlobals.cgPrecacheCheck)
    downloadMap allowDownloadMaps
        | allowDownloadMaps == 0 = do
            configStrings <- use (globals.gCl.csConfigStrings)
            fileExists <- CLParse.checkOrDownloadFile (configStrings Vector.! (Constants.csModels + 1))
            when fileExists $
                proceedWithModels =<< use (clientGlobals.cgPrecacheCheck)
        | otherwise = proceedWithModels =<< use (clientGlobals.cgPrecacheCheck)
    proceedWithModels precacheCheck
        | precacheCheck >= Constants.csModels && precacheCheck < Constants.csModels + Constants.maxModels = do
            allowDownloadModels <- fmap (^.cvValue) allowDownloadModelsCVar
            if allowDownloadModels /= 0
                then do
                    configStrings <- use (globals.gCl.csConfigStrings)
                    done <- downloadModels configStrings precacheCheck
                    unless done finishWithModels
                else
                    finishWithModels
        | otherwise = proceedWithSounds =<< use (clientGlobals.cgPrecacheCheck)
    downloadModels configStrings precacheCheck
        | precacheCheck < Constants.csModels + Constants.maxModels && not (B.null (configStrings Vector.! precacheCheck)) =
            checkWildCardModel (configStrings Vector.! precacheCheck)
        | otherwise = return False
    nextModel = do
        configStrings <- use (globals.gCl.csConfigStrings)
        downloadModels configStrings =<< use (clientGlobals.cgPrecacheCheck)
    checkWildCardModel str
        | BC.head str == '*' || BC.head str == '#' = do
            clientGlobals.cgPrecacheCheck += 1
            nextModel
        | otherwise = checkModelSkin str =<< use (clientGlobals.cgPrecacheModelSkin)
    checkModelSkin str modelSkin
        | modelSkin == 0 = do
            fileExists <- CLParse.checkOrDownloadFile str
            clientGlobals.cgPrecacheModelSkin .= 1
            if fileExists
                then checkModel str =<< use (clientGlobals.cgPrecacheModel)
                else return True
        | otherwise = checkModel str =<< use (clientGlobals.cgPrecacheModel)
    checkModel str Nothing = do
        model <- FS.loadFile str
        clientGlobals.cgPrecacheModel .= model
        maybe couldntLoadModel checkAliasModel model
    checkModel _ (Just model) =
        downloadModel (runGet getDMdlT (BL.fromStrict model)) =<< use (clientGlobals.cgPrecacheModelSkin)
    couldntLoadModel = do
        clientGlobals.cgPrecacheModelSkin .= 0
        clientGlobals.cgPrecacheCheck += 1
        nextModel
    checkAliasModel model
        | B.take 4 model /= idAliasHeader = do
            clientGlobals.cgPrecacheModel .= Nothing
            clientGlobals.cgPrecacheModelSkin .= 0
            clientGlobals.cgPrecacheCheck += 1
            nextModel
        | otherwise = checkAliasVersion (runGet getDMdlT (BL.fromStrict model))
    checkAliasVersion pheader
        | (pheader^.dmVersion) /= Constants.aliasVersion = do
            clientGlobals.cgPrecacheCheck += 1
            clientGlobals.cgPrecacheModelSkin .= 0
            nextModel
        | otherwise = downloadModel pheader =<< use (clientGlobals.cgPrecacheModelSkin)
    downloadModel pheader modelSkin
        | modelSkin - 1 < (pheader^.dmNumSkins) = do
            model <- use (clientGlobals.cgPrecacheModel)
            name <- maybe nameError (getModelName pheader modelSkin) model
            fileExists <- CLParse.checkOrDownloadFile name
            clientGlobals.cgPrecacheModelSkin += 1
            if fileExists
                then downloadModel pheader =<< use (clientGlobals.cgPrecacheModelSkin)
                else return True
        | otherwise = finishModel
    nameError = do
        Com.fatalError "CL.requestNextDownload#downloadModel model is Nothing"
        return B.empty
    getModelName pheader modelSkin model = do
        let name = B.take (Constants.maxSkinName * (pheader^.dmNumSkins)) (B.drop ((pheader^.dmOfsSkins) + (modelSkin - 1) * Constants.maxSkinName) model)
            zeroIdx = B.findIndex (== 0) name
        return (maybe name (\idx -> B.take idx name) zeroIdx)
    finishModel = do
        clientGlobals.cgPrecacheModel .= Nothing
        clientGlobals.cgPrecacheModelSkin .= 0
        clientGlobals.cgPrecacheCheck += 1
        configStrings <- use (globals.gCl.csConfigStrings)
        downloadModels configStrings =<< use (clientGlobals.cgPrecacheCheck)
    finishWithModels = do
        clientGlobals.cgPrecacheCheck .= Constants.csSounds
        proceedWithSounds =<< use (clientGlobals.cgPrecacheCheck)
    proceedWithSounds precacheCheck
        | precacheCheck >= Constants.csSounds && precacheCheck < Constants.csSounds + Constants.maxSounds = do
            allowDownloadSounds <- fmap (^.cvValue) allowDownloadSoundsCVar
            if allowDownloadSounds /= 0
                then do
                    when (precacheCheck == Constants.csSounds) $
                        clientGlobals.cgPrecacheCheck += 1
                    configStrings <- use (globals.gCl.csConfigStrings)
                    done <- downloadSounds configStrings =<< use (clientGlobals.cgPrecacheCheck)
                    unless done finishWithSounds
                else
                    finishWithSounds
        | otherwise = proceedWithImages =<< use (clientGlobals.cgPrecacheCheck)
    downloadSounds configStrings precacheCheck
        | precacheCheck < Constants.csSounds + Constants.maxSounds && not (B.null (configStrings Vector.! precacheCheck)) = do
            let str = configStrings Vector.! precacheCheck
            clientGlobals.cgPrecacheCheck += 1
            if BC.head str == '*'
                then downloadSounds configStrings =<< use (clientGlobals.cgPrecacheCheck)
                else downloadSound ("sound/" `B.append` str)
        | otherwise = return False
    downloadSound name = do
        fileExists <- CLParse.checkOrDownloadFile name
        if fileExists
            then do
                configStrings <- use (globals.gCl.csConfigStrings)
                downloadSounds configStrings =<< use (clientGlobals.cgPrecacheCheck)
            else return True
    finishWithSounds = do
        clientGlobals.cgPrecacheCheck .= Constants.csImages
        proceedWithImages =<< use (clientGlobals.cgPrecacheCheck)
    proceedWithImages precacheCheck
        | precacheCheck >= Constants.csImages && precacheCheck < Constants.csImages + Constants.maxImages = do
            when (precacheCheck == Constants.csImages) $
                clientGlobals.cgPrecacheCheck += 1
            configStrings <- use (globals.gCl.csConfigStrings)
            done <- downloadImages configStrings =<< use (clientGlobals.cgPrecacheCheck)
            unless done finishWithImages
        | otherwise = proceedWithPlayerSkins =<< use (clientGlobals.cgPrecacheCheck)
    downloadImages configStrings precacheCheck
        | precacheCheck < Constants.csImages + Constants.maxImages && not (B.null (configStrings Vector.! precacheCheck)) = do
            let name = B.concat ["pics/", configStrings Vector.! precacheCheck, ".pcx"]
            clientGlobals.cgPrecacheCheck += 1
            fileExists <- CLParse.checkOrDownloadFile name
            if fileExists
                then downloadImages configStrings =<< use (clientGlobals.cgPrecacheCheck)
                else return True
        | otherwise = return False
    finishWithImages = do
        clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins
        proceedWithPlayerSkins =<< use (clientGlobals.cgPrecacheCheck)
    proceedWithPlayerSkins precacheCheck
        | precacheCheck >= Constants.csPlayerSkins && precacheCheck < Constants.csPlayerSkins + Constants.maxClients * playerMult = do
            allowDownloadPlayers <- fmap (^.cvValue) allowDownloadPlayersCVar
            if allowDownloadPlayers /= 0
                then do
                    configStrings <- use (globals.gCl.csConfigStrings)
                    done <- downloadSkins configStrings precacheCheck
                    unless done finishWithPlayerSkins
                else
                    finishWithPlayerSkins
        | otherwise = proceedWithMap =<< use (clientGlobals.cgPrecacheCheck)
    downloadSkins configStrings precacheCheck
        | precacheCheck < Constants.csPlayerSkins + Constants.maxClients * playerMult = do
            let (i, n) = (precacheCheck - Constants.csPlayerSkins) `quotRem` playerMult
                str = configStrings Vector.! (Constants.csPlayerSkins + i)
            downloadSkin i n str
        | otherwise = return False
    downloadSkin i n str
        | B.null str = do
            clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + (i + 1) * playerMult
            configStrings <- use (globals.gCl.csConfigStrings)
            downloadSkins configStrings =<< use (clientGlobals.cgPrecacheCheck)
        | otherwise = do
            let indices = BC.findIndices (== '\\') str
                (pos, pos2) = case indices of
                                  []      -> (0, fromMaybe (-1) (BC.findIndex (== '/') str))
                                  [a]     -> (a, fromMaybe (-1) (BC.findIndex (== '/') str))
                                  (a:b:_) -> (a,b)
                model = B.drop (pos + 1) (B.take pos2 str)
                skin = B.drop (pos2 + 1) str
            done <- startWithModel model skin n i
            if done
                then return True
                else do
                    configStrings <- use (globals.gCl.csConfigStrings)
                    downloadSkins configStrings =<< use (clientGlobals.cgPrecacheCheck)
    startWithModel model skin n i
        | n == 0 = do
            let name = B.concat ["players/", model, "/tris.md2"]
            fileExists <- CLParse.checkOrDownloadFile name
            if fileExists
                then proceedWithWeaponModel model skin (n + 1) i
                else do
                    clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + i * playerMult + 1
                    return True
        | otherwise = proceedWithWeaponModel model skin n i
    proceedWithWeaponModel model skin n i
        | n == 1 = do
            let name = B.concat ["players/", model, "/weapon.md2"]
            fileExists <- CLParse.checkOrDownloadFile name
            if fileExists
                then proceedWithWeaponSkin model skin (n + 1) i
                else do
                    clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + i * playerMult + 2
                    return True
        | otherwise = proceedWithWeaponSkin model skin n i
    proceedWithWeaponSkin model skin n i
        | n == 2 = do
            let name = B.concat ["players/", model, "/weapon.pcx"]
            fileExists <- CLParse.checkOrDownloadFile name
            if fileExists
                then proceedWithSkin model skin (n + 1) i
                else do
                    clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + i * playerMult + 3
                    return True
        | otherwise = proceedWithSkin model skin n i
    proceedWithSkin model skin n i
        | n == 3 = do
            let name = B.concat ["players/", model, "/", skin, ".pcx"]
            fileExists <- CLParse.checkOrDownloadFile name
            if fileExists
                then proceedWithSkinI model skin (n + 1) i
                else do
                    clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + i * playerMult + 4
                    return True
        | otherwise = proceedWithSkinI model skin n i
    proceedWithSkinI model skin n i
        | n == 4 = do
            let name = B.concat ["players/", model, "/", skin, "_i.pcx"]
            fileExists <- CLParse.checkOrDownloadFile name
            if fileExists
                then return False
                else do
                    clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + i * playerMult + 5
                    return True
        | otherwise = return False
    finishWithPlayerSkins = do
        clientGlobals.cgPrecacheCheck .= envCnt
        proceedWithMap =<< use (clientGlobals.cgPrecacheCheck)
    proceedWithMap precacheCheck
        | precacheCheck == envCnt = do
            clientGlobals.cgPrecacheCheck += 1
            configStrings <- use (globals.gCl.csConfigStrings)
            (_, checksum) <- CM.loadMap (configStrings Vector.! (Constants.csModels + 1)) True [0]
            let mapChecksum = head checksum -- IMPROVE: kinda ugly
                chk = configStrings Vector.! Constants.csMapChecksum
            if mapChecksum `xor` Lib.atoi chk /= 0
                then Com.comError Constants.errDrop
                                  (B.concat ["Local map version differs from server: ", encode mapChecksum, " != '", chk, "'\n"])
                else proceedWithSky =<< use (clientGlobals.cgPrecacheCheck)
        | otherwise = proceedWithSky =<< use (clientGlobals.cgPrecacheCheck)
    proceedWithSky precacheCheck
        | precacheCheck > envCnt && precacheCheck < textureCnt = do
            allowDownload <- fmap (^.cvValue) allowDownloadCVar
            allowDownloadMaps <- fmap (^.cvValue) allowDownloadMapsCVar
            if allowDownload /= 0 && allowDownloadMaps /= 0
                then do
                    configStrings <- use (globals.gCl.csConfigStrings)
                    done <- downloadSky (configStrings Vector.! Constants.csSky) precacheCheck
                    unless done finishWithSky
                else finishWithSky
        | otherwise = proceedWithPrecacheCheck =<< use (clientGlobals.cgPrecacheCheck)
    downloadSky skyStr precacheCheck
        | precacheCheck < textureCnt = do
            let n = precacheCheck - envCnt - 1
                name = B.concat ["env/", skyStr, envSuf Vector.! (n `div` 2), if n .&. 1 /= 0 then ".pcx" else ".tga"]
            clientGlobals.cgPrecacheCheck += 1
            fileExists <- CLParse.checkOrDownloadFile name
            if fileExists
                then downloadSky skyStr =<< use (clientGlobals.cgPrecacheCheck)
                else return True
        | otherwise = return False
    finishWithSky = do
        clientGlobals.cgPrecacheCheck .= textureCnt
        proceedWithPrecacheCheck =<< use (clientGlobals.cgPrecacheCheck)
    proceedWithPrecacheCheck :: Int -> Quake ()
    proceedWithPrecacheCheck precacheCheck
        | precacheCheck == textureCnt = do
            clientGlobals.cgPrecacheCheck .= textureCnt + 1
            clientGlobals.cgPrecacheTex .= 0
            proceedWithTextures =<< use (clientGlobals.cgPrecacheCheck)
        | otherwise = proceedWithTextures =<< use (clientGlobals.cgPrecacheCheck)
    proceedWithTextures precacheCheck
        | precacheCheck == textureCnt + 1 = do
            allowDownload <- fmap (^.cvValue) allowDownloadCVar
            allowDownloadMaps <- fmap (^.cvValue) allowDownloadMapsCVar
            if allowDownload /= 0 && allowDownloadMaps /= 0
                then do
                    mapSurfaces <- use (cmGlobals.cmMapSurfaces)
                    numTexInfo <- use (cmGlobals.cmNumTexInfo)
                    done <- downloadTextures mapSurfaces numTexInfo =<< use (clientGlobals.cgPrecacheTex)
                    unless done finishWithTextures
                else finishWithTextures
        | otherwise = finishDownloads
    downloadTextures mapSurfaces numTexInfo precacheTex
        | precacheTex < numTexInfo = do
            let name = B.concat ["textures/", fromMaybe B.empty ((mapSurfaces Vector.! precacheTex)^.msRName), ".wal"] -- IMPROVE: is B.empty ok here? maybe an error should be here?
            clientGlobals.cgPrecacheTex += 1
            fileExists <- CLParse.checkOrDownloadFile name
            if fileExists
                then downloadTextures mapSurfaces numTexInfo =<< use (clientGlobals.cgPrecacheTex)
                else return True
        | otherwise = return False
    finishWithTextures = do
        clientGlobals.cgPrecacheCheck .= textureCnt + 999
        finishDownloads
    finishDownloads = do
        CLParse.registerSounds
        CLView.prepRefresh
        precacheSpawnCount <- use (clientGlobals.cgPrecacheSpawnCount)
        MSG.writeByteI (globals.gCls.csNetChan.ncMessage) Constants.clcStringCmd
        MSG.writeString (globals.gCls.csNetChan.ncMessage) (B.concat ["begin ", encode precacheSpawnCount, "\n"])
