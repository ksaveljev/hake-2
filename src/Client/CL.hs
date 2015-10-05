{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Client.CL where

import Control.Concurrent (threadDelay)
import Control.Lens (use, (.=), (^.), (+=), preuse, ix, zoom, (&), (.~))
import Control.Monad (unless, liftM, when, void)
import Data.Bits ((.|.), xor, (.&.))
import Data.Maybe (fromJust, isNothing)
import Linear (V4(..))
import System.IO (IOMode(ReadWriteMode), hSeek, hSetFileSize, SeekMode(AbsoluteSeek))
import System.Mem (performGC)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as Vec

import Quake
import QuakeState
import CVarVariables
import Client.CheatVarT
import QCommon.QFiles.MD2.DMdlT
import QCommon.XCommandT
import qualified Constants
import qualified Client.CLFX as CLFX
import {-# SOURCE #-} qualified Client.CLInput as CLInput
import {-# SOURCE #-} qualified Client.CLParse as CLParse
import qualified Client.CLPred as CLPred
import qualified Client.CLTEnt as CLTEnt
import qualified Client.CLView as CLView
import qualified Client.Console as Console
import qualified Client.Key as Key
import qualified Client.Menu as Menu
import qualified Client.SCR as SCR
import qualified Client.V as V
import qualified Client.VID as VID
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified Game.Info as Info
import qualified QCommon.CBuf as CBuf
import qualified QCommon.CM as CM
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.FS as FS
import qualified QCommon.MSG as MSG
import qualified QCommon.NetChannel as NetChannel
import qualified QCommon.SZ as SZ
import qualified Server.SVMain as SVMain
import qualified Sound.S as S
import qualified Sys.IN as IN
import qualified Sys.NET as NET
import qualified Sys.Sys as Sys
import qualified Sys.Timer as Timer
import qualified Util.Lib as Lib

playerMult :: Int
playerMult = 5

-- ENV_CNT is map load, ENV_CNT+1 is first env map
envCnt :: Int
envCnt = Constants.csPlayerSkins + Constants.maxClients * playerMult

textureCnt :: Int
textureCnt = envCnt + 13

envSuf :: Vec.Vector B.ByteString
envSuf = Vec.fromList [ "rt", "bk", "lf", "ft", "up", "dn" ]

cheatVars :: Vec.Vector CheatVarT
cheatVars =
    Vec.fromList [ CheatVarT "timescale" "1"
                 , CheatVarT "timedemo" "0"
                 , CheatVarT "r_drawworld" "1"
                 , CheatVarT "cl_testlights" "0"
                 , CheatVarT "r_fullbright" "0"
                 , CheatVarT "r_drawflat" "0"
                 , CheatVarT "paused" "0"
                 , CheatVarT "fixedtime" "0"
                 , CheatVarT "sw_draworder" "0"
                 , CheatVarT "gl_lightmap" "0"
                 , CheatVarT "gl_saturatelighting" "0"
                 ]

-- Initialize client subsystem.
init :: Quake ()
init = do
    dedicatedValue <- liftM (^.cvValue) dedicatedCVar

    unless (dedicatedValue /= 0) $ do
      Console.init >> S.init >> VID.init >> V.init

      bufData <- use $ globals.netMessageBuffer
      globals.netMessage.sbData .= bufData
      globals.netMessage.sbMaxSize .= B.length bufData

      Menu.init >> SCR.init >> initLocal >> IN.init

      FS.execAutoexec
      CBuf.execute

initLocal :: Quake ()
initLocal = do
    globals.cls.csState .= Constants.caDisconnected
    msec <- Timer.milliseconds
    globals.cls.csRealTime .= msec

    CLInput.initInput

    void $ CVar.get "adr0" "" Constants.cvarArchive
    void $ CVar.get "adr1" "" Constants.cvarArchive
    void $ CVar.get "adr2" "" Constants.cvarArchive
    void $ CVar.get "adr3" "" Constants.cvarArchive
    void $ CVar.get "adr4" "" Constants.cvarArchive
    void $ CVar.get "adr5" "" Constants.cvarArchive
    void $ CVar.get "adr6" "" Constants.cvarArchive
    void $ CVar.get "adr7" "" Constants.cvarArchive
    void $ CVar.get "adr8" "" Constants.cvarArchive

    -- register our variables
    void $ CVar.get "cl_stereo_separation" "0.4" Constants.cvarArchive
    void $ CVar.get "cl_stereo" "0" 0

    void $ CVar.get "cl_blend" "1" 0
    void $ CVar.get "cl_lights" "1" 0
    void $ CVar.get "cl_particles" "1" 0
    void $ CVar.get "cl_entities" "1" 0
    void $ CVar.get "cl_gun" "1" 0
    void $ CVar.get "cl_footsteps" "1" 0
    void $ CVar.get "cl_noskins" "0" 0
    void $ CVar.get "cl_autoskins" "0" 0
    void $ CVar.get "cl_predict" "1" 0

    void $ CVar.get "cl_maxfps" "90" 0

    void $ CVar.get "cl_upspeed" "200" 0
    void $ CVar.get "cl_forwardspeed" "200" 0
    void $ CVar.get "cl_sidespeed" "200" 0
    void $ CVar.get "cl_yawspeed" "140" 0
    void $ CVar.get "cl_pitchspeed" "150" 0
    void $ CVar.get "cl_anglespeedkey" "1.5" 0

    void $ CVar.get "cl_run" "0" Constants.cvarArchive
    void $ CVar.get "lookspring" "0" Constants.cvarArchive
    void $ CVar.get "lookstrafe" "0" Constants.cvarArchive
    void $ CVar.get "sensitivity" "3" Constants.cvarArchive

    void $ CVar.get "m_pitch" "0.022" Constants.cvarArchive
    void $ CVar.get "m_yaw" "0.022" 0
    void $ CVar.get "m_forward" "1" 0
    void $ CVar.get "m_side" "1" 0

    void $ CVar.get "cl_shownet" "0" 0
    void $ CVar.get "cl_showmiss" "0" 0
    void $ CVar.get "showclamp" "0" 0
    void $ CVar.get "cl_timeout" "120" 0
    void $ CVar.get "paused" "0" 0
    void $ CVar.get "timedemo" "0" 0

    void $ CVar.get "rcon_password" "" 0
    void $ CVar.get "rcon_address" "" 0

    void $ CVar.get "r_lightlevel" "0" 0

    -- userinfo
    void $ CVar.get "password" "" Constants.cvarUserInfo
    void $ CVar.get "spectator" "0" Constants.cvarUserInfo
    void $ CVar.get "name" "unnamed" (Constants.cvarUserInfo .|. Constants.cvarArchive)
    void $ CVar.get "skin" "male/grunt" (Constants.cvarUserInfo .|. Constants.cvarArchive)
    void $ CVar.get "rate" "25000" (Constants.cvarUserInfo .|. Constants.cvarArchive) -- FIXME
    void $ CVar.get "msg" "1" (Constants.cvarUserInfo .|. Constants.cvarArchive)
    void $ CVar.get "hand" "0" (Constants.cvarUserInfo .|. Constants.cvarArchive)
    void $ CVar.get "fov" "90" (Constants.cvarUserInfo .|. Constants.cvarArchive)
    void $ CVar.get "gender" "male" (Constants.cvarUserInfo .|. Constants.cvarArchive)
    void $ CVar.get "gender_auto" "1" Constants.cvarArchive

    -- clear this so we know when user sets it manually
    gender <- genderCVar
    CVar.update gender { _cvModified = False }

    void $ CVar.get "cl_vwep" "1" Constants.cvarArchive

    -- register our commands
    Cmd.addCommand "cmd" (Just forwardToServerF)
    Cmd.addCommand "pause" (Just pauseF)
    Cmd.addCommand "pingservers" (Just pingServersF)
    Cmd.addCommand "skins" (Just skinsF)

    Cmd.addCommand "userinfo" (Just userInfoF)
    Cmd.addCommand "snd_restart" (Just sndRestartF)

    Cmd.addCommand "changing" (Just changingF)
    Cmd.addCommand "disconnect" (Just disconnectF)
    Cmd.addCommand "record" (Just recordF)
    Cmd.addCommand "stop" (Just stopF)

    Cmd.addCommand "quit" (Just quitF)

    Cmd.addCommand "connect" (Just connectF)
    Cmd.addCommand "reconnect" (Just reconnectF)

    Cmd.addCommand "rcon" (Just rconF)

    Cmd.addCommand "precache" (Just precacheF)

    Cmd.addCommand "download" (Just CLParse.downloadF)

    --
    -- forward to server commands
    --
    -- the only thing this does is allow command completion
    -- to work -- all unknown commands are automatically
    -- forwarded to the server
    Cmd.addCommand "wave" Nothing
    Cmd.addCommand "inven" Nothing
    Cmd.addCommand "kill" Nothing
    Cmd.addCommand "use" Nothing
    Cmd.addCommand "drop" Nothing
    Cmd.addCommand "say" Nothing
    Cmd.addCommand "say_team" Nothing
    Cmd.addCommand "info" Nothing
    Cmd.addCommand "prog" Nothing
    Cmd.addCommand "give" Nothing
    Cmd.addCommand "god" Nothing
    Cmd.addCommand "notarget" Nothing
    Cmd.addCommand "noclip" Nothing
    Cmd.addCommand "invuse" Nothing
    Cmd.addCommand "invprev" Nothing
    Cmd.addCommand "invnext" Nothing
    Cmd.addCommand "invdrop" Nothing
    Cmd.addCommand "weapnext" Nothing
    Cmd.addCommand "weapprev" Nothing

{-
- WriteConfiguration
- 
- Writes key bindings and archived cvars to config.cfg.
-}
writeConfiguration :: Quake ()
writeConfiguration = do
    gamedir <- FS.gameDir
    let path = gamedir `B.append` "/config.cfg"

    f <- Lib.fOpen path ReadWriteMode

    case f of
      Nothing -> do
        Com.printf "Couldn't write config.cfg.\n"
      Just h -> do
        -- IMPROVE: catch exceptions here?
        io $ hSeek h AbsoluteSeek 0
        io $ hSetFileSize h 0

        io $ B.hPut h "// generated by quake, do not modify\n"

        Key.writeBindings h
        Lib.fClose h
        CVar.writeVariables path

frame :: Int -> Quake ()
frame msec = do
    dedicatedValue <- liftM (^.cvValue) dedicatedCVar

    unless (dedicatedValue /= 0) $ do
      clientGlobals.cgExtraTime += msec

      skip <- shouldSkip

      unless skip $ do
        -- let the mouse activate or deactivate
        IN.frame

        -- decide the simulation time
        extraTime <- use $ clientGlobals.cgExtraTime
        time <- use $ globals.curtime
        let frameTime = fromIntegral extraTime / 1000
        globals.cls.csFrameTime .= frameTime
        globals.cl.csTime += extraTime
        globals.cls.csRealTime .= time

        clientGlobals.cgExtraTime .= 0

        when (frameTime > (1 / 5)) $
          globals.cls.csFrameTime .= 1 / 5

        -- if in the debugger last frame, don't timeout
        when (msec > 5000) $ do
          millis <- Timer.milliseconds
          globals.cls.csNetChan.ncLastReceived .= millis
          
        -- fetch results from server
        readPackets

        -- send a new command message to the server
        sendCommand

        -- predict all unacknowledged movements
        CLPred.predictMovement

        -- allow rendering DLL change
        VID.checkChanges
        state <- use $ globals.cls.csState
        refreshPrepped <- use $ globals.cl.csRefreshPrepped
        when (not refreshPrepped && state == Constants.caActive) $ do
          CLView.prepRefresh
          -- force GC after level loading
          -- but not on playing a cinematic
          cinematicTime <- use $ globals.cl.csCinematicTime
          when (cinematicTime == 0) $
            io performGC -- TODO: do we really need this?

        SCR.updateScreen

        -- update audio
        updateAudio
        
        -- advance local effects for next frame
        CLFX.runDLights
        CLFX.runLightStyles
        SCR.runCinematic
        SCR.runConsole

        globals.cls.csFrameCount += 1

        keyDest <- use $ globals.cls.csKeyDest

        when (state /= Constants.caActive || keyDest /= Constants.keyGame) $
          io (threadDelay $ 20 * 1000)

  where shouldSkip :: Quake Bool
        shouldSkip = do
          timeDemoValue <- liftM (^.cvValue) clTimeDemoCVar
          maxFpsValue <- liftM (^.cvValue) clMaxFPSCVar
          state <- use $ globals.cls.csState
          extraTime <- use $ clientGlobals.cgExtraTime

          if timeDemoValue == 0
                    -- don't flood packets out while connecting            -- framerate is too high
            then if (state == Constants.caConnected && extraTime < 100) || (fromIntegral extraTime < 1000 / maxFpsValue)
                   then return True
                   else return False
            else return False

        updateAudio :: Quake ()
        updateAudio = do
          cl' <- use $ globals.cl
          S.update (cl'^.csRefDef.rdViewOrg) (cl'^.csVForward) (cl'^.csVRight) (cl'^.csVUp)


-- Called after an ERR_DROP was thrown.
drop :: Quake ()
drop = do
    clientStatic <- use $ globals.cls
    let state = clientStatic^.csState

    when (state /= Constants.caUninitialized && state /= Constants.caDisconnected) $ do
      disconnect

      -- drop loading plaque unless this is the initial game start
      when ((clientStatic^.csDisableServerCount) /= -1) $
        SCR.endLoadingPlaque -- get rid of loading plaque

{-
- Disconnect
- 
- Goes from a connected state to full screen console state Sends a
- disconnect message to the server This is also called on Com_Error, so it
- shouldn't cause any errors.
-}
disconnect :: Quake ()
disconnect = do
    cl' <- use $ globals.cl
    cls' <- use $ globals.cls

    unless ((cls'^.csState) == Constants.caDisconnected) $ do
      timeDemoValue <- liftM (^.cvValue) clTimeDemoCVar

      when (timeDemoValue /= 0) $ do
        ms <- Timer.milliseconds
        
        let time = fromIntegral (ms - (cl'^.csTimeDemoStart)) :: Float

        when (time > 0) $
          Com.printf (BC.pack (show (cl'^.csTimeDemoFrames)) `B.append` " frames, " `B.append` BC.pack (show (time / 1000.0)) `B.append` " seconds: " `B.append` BC.pack (show (fromIntegral (cl'^.csTimeDemoFrames) * 1000.0 / time)) `B.append` " fps\n") -- IMPROVE?

      globals.cl.csRefDef.rdBlend .= V4 0 0 0 0
      
      Just renderer <- use $ globals.re
      (renderer^.rRefExport.reCinematicSetPalette) Nothing

      Menu.forceMenuOff

      globals.cls.csConnectTime .= 0

      SCR.stopCinematic

      when (cls'^.csDemoRecording) $
        stopF

      -- send a disconnect message to the server
      let fin = B.pack [fromIntegral Constants.clcStringCmd] `B.append` "disconnect"
      NetChannel.transmit (globals.cls.csNetChan) (B.length fin) fin
      NetChannel.transmit (globals.cls.csNetChan) (B.length fin) fin
      NetChannel.transmit (globals.cls.csNetChan) (B.length fin) fin

      clearState

      -- stop download
      case cls'^.csDownload of
        Nothing -> return ()
        Just handle -> do
          Lib.fClose handle
          globals.cls.csDownload .= Nothing

      globals.cls.csState .= Constants.caDisconnected

forwardToServerF :: XCommandT
forwardToServerF = do
    state <- use $ globals.cls.csState

    if state /= Constants.caConnected && state /= Constants.caActive
      then do
        v0 <- Com.argv 0
        Com.printf $ "Can't \"" `B.append` v0 `B.append` "\", not connected\n"
      else do
        -- don't forward the first argument
        c <- Cmd.argc
        when (c > 1) $ do
          args <- Cmd.args
          MSG.writeByteI (globals.cls.csNetChan.ncMessage) Constants.clcStringCmd
          SZ.print (globals.cls.csNetChan.ncMessage) args

pauseF :: XCommandT
pauseF = do
    maxClientsValue <- CVar.variableValue "maxclients"
    state <- use $ globals.serverState

    if maxClientsValue > 1 || state == 0
      then
        CVar.setValueI "paused" 0
      else do
        pausedValue <- liftM (^.cvValue) clPausedCVar
        CVar.setValueF "paused" pausedValue

pingServersF :: XCommandT
pingServersF = do
    NET.config True -- allow remote

    -- send a broadcast packet
    Com.printf "pinging boardcast...\n"

    adr <- checkNoUDP newNetAdrT >>= checkNoIPX

    -- send a packet to each address book entry
    pingAddressBook adr 0 16

  where checkNoUDP :: NetAdrT -> Quake NetAdrT
        checkNoUDP adr = do
          Just noUDP <- CVar.get "noudp" "0" Constants.cvarNoSet

          if (noUDP^.cvValue) == 0
            then do
              -- TODO: do we need port conversion? adr.port = BigShort (PORT_SERVER)
              let adr' = adr & naType .~ Constants.naBroadcast
                             & naPort .~ Constants.portServer

              NetChannel.outOfBandPrint Constants.nsClient adr' ("info " `B.append` BC.pack (show Constants.protocolVersion)) -- IMPROVE?
              return adr'

            else
              return adr

        checkNoIPX :: NetAdrT -> Quake NetAdrT
        checkNoIPX adr = do
          Just noIPX <- CVar.get "noipx" "1" Constants.cvarNoSet

          if (noIPX^.cvValue) == 0
            then do
              -- TODO: do we need port conversion? adr.port = BigShort (PORT_SERVER)
              let adr' = adr & naType .~ Constants.naBroadcastIpx
                             & naPort .~ Constants.portServer

              NetChannel.outOfBandPrint Constants.nsClient adr' ("info " `B.append` BC.pack (show Constants.protocolVersion)) -- IMPROVE?
              return adr'

            else
              return adr

        pingAddressBook :: NetAdrT -> Int -> Int -> Quake ()
        pingAddressBook adr idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let name = "adr" `B.append` BC.pack (show idx) -- IMPROVE?
              adrString <- CVar.variableString name

              if B.null adrString
                then
                  pingAddressBook adr (idx + 1) maxIdx

                else do
                  Com.printf ("pinging " `B.append` adrString `B.append` "...\n")

                  maybeAdr <- NET.stringToAdr adrString

                  case maybeAdr of
                    Nothing -> do
                      Com.printf ("Bad address: " `B.append` adrString `B.append` "\n")
                      pingAddressBook adr (idx + 1) maxIdx

                    Just adr' -> do
                      let adr'' = if (adr'^.naPort) == 0
                                    then adr' & naPort .~ Constants.portServer
                                    else adr'

                      NetChannel.outOfBandPrint Constants.nsClient adr'' ("info " `B.append` BC.pack (show Constants.protocolVersion)) -- IMPROVE?
                      pingAddressBook adr'' (idx + 1) maxIdx

{-
- Skins_f
- 
- Load or download any custom player skins and models.
-}
skinsF :: XCommandT
skinsF = do
    configStrings <- use $ globals.cl.csConfigStrings
    loadOrDownload configStrings 0 Constants.maxClients

  where loadOrDownload :: Vec.Vector B.ByteString -> Int -> Int -> XCommandT
        loadOrDownload configStrings idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              if B.null (configStrings Vec.! (Constants.csPlayerSkins + idx)) -- TODO: jake compares to NULL here
                then
                  loadOrDownload configStrings (idx + 1) maxIdx
                else do
                  Com.printf ("client " `B.append` BC.pack (show idx) `B.append` ": " `B.append` (configStrings Vec.! (Constants.csPlayerSkins + idx)) `B.append` "\n")
                  SCR.updateScreen
                  Sys.sendKeyEvents -- pump message loop
                  CLParse.parseClientInfo idx

userInfoF :: XCommandT
userInfoF = do
    Com.printf "User info settings:\n"
    userInfo <- CVar.userInfo
    Info.print userInfo

{-
- Snd_Restart_f
- 
- Restart the sound subsystem so it can pick up new parameters and flush
- all sounds.
-}
sndRestartF :: XCommandT
sndRestartF = do
    S.shutdown
    S.init
    CLParse.registerSounds

-- Just sent as a hint to the client that they should drop to full console.
changingF :: XCommandT
changingF = do
    -- if we are downloading, we don't change!
    -- This is so we don't suddenly stop downloading a map
    download <- use $ globals.cls.csDownload

    when (isNothing download) $ do
      SCR.beginLoadingPlaque
      globals.cls.csState .= Constants.caConnected -- not active anymore, but not disconnected

      Com.printf "\nChanging map...\n"

disconnectF :: XCommandT
disconnectF = Com.comError Constants.errDrop "Disconnected from server"

recordF :: XCommandT
recordF = io (putStrLn "CL.recordF") >> undefined -- TODO

stopF :: XCommandT
stopF = io (putStrLn "CL.stopF") >> undefined -- TODO

quitF :: XCommandT
quitF = do
    disconnect
    Com.quit

connectF :: XCommandT
connectF = do
    c <- Cmd.argc

    if c /= 2
      then
        Com.printf "usage: connect <server>\n"
      else do
        ss <- use $ globals.serverState

        if ss /= 0
          then SVMain.shutdown "Server quit\n" False -- if running a local server, kill it and reissue
          else disconnect

        server <- Cmd.argv 1

        NET.config True -- allow remote

        disconnect

        zoom (globals.cls) $ do
          csState .= Constants.caConnecting
          csServerName .= server
          csConnectTime .= -99999

        -- CL_CheckForResend() will fire immediately

reconnectF :: XCommandT
reconnectF = io (putStrLn "CL.reconnectF") >> undefined -- TODO

rconF :: XCommandT
rconF = io (putStrLn "CL.rconF") >> undefined -- TODO

{-
- The server will send this command right before allowing the client into
- the server.
-}
precacheF :: XCommandT
precacheF = do
    c <- Cmd.argc

    -- Yet another hack to let old demos work the old precache sequence
    when (c < 2) $ do
      io (putStrLn "CL.precacheF") >> undefined -- TODO

    v1 <- Cmd.argv 1

    zoom clientGlobals $ do
      cgPrecacheCheck .= Constants.csModels
      cgPrecacheSpawnCount .= Lib.atoi v1
      cgPrecacheModel .= Nothing
      cgPrecacheModelSkin .= 0

    requestNextDownload

remoteCommandHeader :: B.ByteString
remoteCommandHeader = B.pack [0xFF, 0xFF, 0xFF, 0xFF]

readPackets :: Quake ()
readPackets = do
    nextPacket

    -- check timeout
    cls' <- use $ globals.cls
    timeoutValue <- liftM (^.cvValue) clTimeoutCVar
    if (cls'^.csState) >= Constants.caConnected && (cls'^.csRealTime) - (cls'^.csNetChan.ncLastReceived) > (truncate timeoutValue) * 1000
      then do
        globals.cl.csTimeOutCount += 1
        use (globals.cl.csTimeOutCount) >>= \v ->
          when (v > 5) $ do -- timeoutcount saves debugger
            Com.printf "\nServer connection timed out.\n"
            disconnect
      else
        globals.cl.csTimeOutCount .= 0

  where nextPacket :: Quake ()
        nextPacket = do
          gotPacket <- NET.getPacket Constants.nsClient (globals.netFrom) (globals.netMessage)
          cls' <- use $ globals.cls

          when gotPacket $ do
            netMsg <- use $ globals.netMessage

            if | B.take 4 (netMsg^.sbData) == remoteCommandHeader -> do
                   connectionlessPacket
               | (cls'^.csState) == Constants.caDisconnected || (cls'^.csState) == Constants.caConnecting ->
                   -- dump it if not connected
                   return ()
               | (netMsg^.sbCurSize) < 8 -> do
                   from <- use $ globals.netFrom
                   Com.printf $ (NET.adrToString from) `B.append` ": Runt packet\n"
               | otherwise -> do
                   -- packet from server
                   from <- use $ globals.netFrom
                   remote <- use $ globals.cls.csNetChan.ncRemoteAddress
                   let same = NET.compareAdr from remote

                   if not same
                     then Com.dprintf $ (NET.adrToString from) `B.append` ":sequenced packet without connection\n"
                     else do
                       ok <- NetChannel.process (globals.cls.csNetChan) (globals.netMessage)
                       -- might not be accepted for some reason
                       when ok $
                         CLParse.parseServerMessage

            nextPacket

sendCommand :: Quake ()
sendCommand = do
    -- get new key events
    Sys.sendKeyEvents

    -- allow mice or other external controllers to add commands
    IN.commands

    -- process console commands
    CBuf.execute

    -- fix any cheating cvars
    fixCVarCheats

    -- send intentions now
    CLInput.sendCmd

    -- resend a connection request if necessary
    checkForResend

fixCVarCheats :: Quake ()
fixCVarCheats = do
    Just maxClients <- preuse $ globals.cl.csConfigStrings.ix Constants.csMaxClients

    -- single player can cheat
    unless (maxClients == "1" || B.length maxClients == 0) $ do
      -- -----------------------------------
      -- ACTUALLY WE DO NOT NEED THIS REALLY
      -- -----------------------------------
      -- find all the cvars if we haven't done it yet
      {-
      numCheatVars <- use $ clientGlobals.cgNumCheatVars
      when (numCheatVars == 0) $ do
        undefined -- TODO
      -}

      -- make sure they are all set to the proper values
      Vec.mapM_ fixCVar cheatVars

  where fixCVar :: CheatVarT -> Quake ()
        fixCVar cheatVar = do
          cvar <- CVar.getExisting (cheatVar^.chvName)
          when ((cvar^.cvString) /= (cheatVar^.chvValue)) $
            void $ CVar.set (cheatVar^.chvName) (cheatVar^.chvValue)

{-
- CheckForResend
- 
- Resend a connect message if the last one has timed out.
-}
checkForResend :: Quake ()
checkForResend = do
    -- if the local server is running and we aren't then connect
    state <- use $ globals.cls.csState
    serverState' <- use $ globals.serverState

    if state == Constants.caDisconnected && serverState' /= 0
      then do
        globals.cls.csState .= Constants.caConnecting
        globals.cls.csServerName .= "localhost"
        -- we don't need a challenge on the localhost
        sendConnectPacket
      else do
        realTime <- use $ globals.cls.csRealTime
        connectTime <- use $ globals.cls.csConnectTime

        -- resend if we haven't gotten a reply yet
        unless (state /= Constants.caConnecting || (fromIntegral realTime - connectTime) < 3000) $ do
          serverName <- use $ globals.cls.csServerName
          adrMaybe <- NET.stringToAdr serverName

          case adrMaybe of
            Nothing -> do
              Com.printf "Bad server address\n"
              globals.cls.csState .= Constants.caDisconnected
            Just adr -> do
              let adr' = if (adr^.naPort) == 0
                           then adr { _naPort = Constants.portServer }
                           else adr

              -- for retransmit requests
              globals.cls.csConnectTime .= fromIntegral realTime

              Com.printf $ "Connecting to " `B.append` serverName `B.append` "...\n"

              NetChannel.outOfBandPrint Constants.nsClient adr' "getchallenge\n"

fixUpGender :: Quake ()
fixUpGender = io (putStrLn "CL.fixUpGender") >> undefined -- TODO

{-
- SendConnectPacket
- 
- We have gotten a challenge from the server, so try and connect.
-}
sendConnectPacket :: Quake ()
sendConnectPacket = do
    serverName <- use $ globals.cls.csServerName

    maybeAdr <- NET.stringToAdr serverName

    case maybeAdr of
      Nothing -> do
        Com.printf "Bad server address\n"
        globals.cls.csConnectTime .= 0
      Just adr -> do
        let adr' = if (adr^.naPort) == 0
                     then adr { _naPort = Constants.portServer }
                     else adr

        port :: Int <- liftM truncate (CVar.variableValue "qport")
        globals.userInfoModified .= False

        challenge <- use $ globals.cls.csChallenge
        userInfo <- CVar.userInfo

        let str = "connect " `B.append` (BC.pack $ show Constants.protocolVersion) `B.append` -- IMPROVE?
                  " " `B.append` (BC.pack $ show port) `B.append` " " `B.append` -- IMPROVE ?
                  (BC.pack $ show challenge) `B.append` " \"" `B.append` userInfo `B.append` "\"\n" -- IMPROVE?

        NetChannel.outOfBandPrint Constants.nsClient adr' str

connectionlessPacket :: Quake ()
connectionlessPacket = do
    MSG.beginReading (globals.netMessage)
    _ <- MSG.readLong (globals.netMessage) -- skip the -1

    s <- MSG.readStringLine (globals.netMessage)

    Cmd.tokenizeString s False

    c <- Cmd.argv 0

    use (globals.netFrom) >>= \from ->
      Com.println $ NET.adrToString from `B.append` ": " `B.append` c

    case c of
      -- server connection
      "client_connect" -> do
        state <- use $ globals.cls.csState
        if state == Constants.caConnected
          then 
            Com.printf "Dup connect received.  Ignored.\n"
          else do
            from <- use $ globals.netFrom
            qport <- use $ globals.cls.csQuakePort
            NetChannel.setup Constants.nsClient (globals.cls.csNetChan) from qport
            MSG.writeCharI (globals.cls.csNetChan.ncMessage) (fromIntegral Constants.clcStringCmd)
            MSG.writeString (globals.cls.csNetChan.ncMessage) "new"
            globals.cls.csState .= Constants.caConnected

      -- server responding to a status broadcast
      "info" ->
        parseStatusMessage

      -- remote command from gui front end
      "cmd" -> do
        from <- use $ globals.netFrom

        if not (NET.isLocalAddress from)
          then
            Com.printf "Command packet from remote host.  Ignored.\n"
          else do
            cmd <- MSG.readString (globals.netMessage)
            CBuf.addText cmd
            CBuf.addText "\n"

      -- print command from somewhere
      "print" -> do
        msg <- MSG.readString (globals.netMessage)
        when (B.length msg > 0) $
          Com.printf msg

      -- ping from somewhere
      "ping" -> do
        from <- use $ globals.netFrom
        NetChannel.outOfBandPrint Constants.nsClient from "ack"

      -- challenge from the server we are connecting to
      "challenge" -> do
        v1 <- Cmd.argv 1
        globals.cls.csChallenge .= Lib.atoi v1
        sendConnectPacket

      -- echo request from server
      "echo" -> do
        v1 <- Cmd.argv 1
        from <- use $ globals.netFrom
        NetChannel.outOfBandPrint Constants.nsClient from v1

      _ -> Com.printf "Unknown command.\n"

parseStatusMessage :: Quake ()
parseStatusMessage = io (putStrLn "CL.parseStatusMessage") >> undefined -- TODO

writeDemoMessage :: Quake ()
writeDemoMessage = io (putStrLn "CL.writeDemoMessage") >> undefined -- TODO

clearState :: Quake ()
clearState = do
    S.stopAllSounds
    CLFX.clearEffects
    CLTEnt.clearTEnts

    -- wipe the entire cl structure
    globals.cl .= newClientStateT
    globals.clEntities .= Vec.replicate Constants.maxEdicts newCEntityT

    SZ.clear (globals.cls.csNetChan.ncMessage)

requestNextDownload :: Quake ()
requestNextDownload = do
    state <- use $ globals.cls.csState

    when (state == Constants.caConnected) $ do
      updatePrecacheCheck

      checkIfDownloadStarted
        >>= checkModelsDownload
        >>= checkSoundsDownload
        >>= checkImagesDownload
        >>= checkSkinsDownload
        >>= loadMap
        >>= checkPicsDownload
        >>= updatePrecacheCheck2
        >>= checkTexturesDownload
        >>= finishDownloads

  where updatePrecacheCheck :: Quake ()
        updatePrecacheCheck = do
          allowDownloadValue <- liftM (^.cvValue) allowDownloadCVar
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          when (allowDownloadValue == 0 && precacheCheck < envCnt) $
            clientGlobals.cgPrecacheCheck .= envCnt

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        checkIfDownloadStarted :: Quake Bool
        checkIfDownloadStarted = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          if precacheCheck == Constants.csModels -- confirm map
            then do
              clientGlobals.cgPrecacheCheck .= Constants.csModels + 2 -- 0 isn't used
              allowDownloadMapsValue <- liftM (^.cvValue) allowDownloadMapsCVar

              if allowDownloadMapsValue /= 0
                then do
                  Just str <- preuse $ globals.cl.csConfigStrings.ix (Constants.csModels + 1)
                  fileExists <- CLParse.checkOrDownloadFile str
                  return $ if fileExists
                             then False
                             else True -- started a download
                else
                  return False
            else
              return False

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        checkModelsDownload :: Bool -> Quake Bool
        checkModelsDownload True = return True -- do nothing
        checkModelsDownload False = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          if precacheCheck >= Constants.csModels && precacheCheck < Constants.csModels + Constants.maxModels
            then do
              allowDownloadModelsValue <- liftM (^.cvValue) allowDownloadModelsCVar

              if allowDownloadModelsValue /= 0
                then do
                  configStrings <- use $ globals.cl.csConfigStrings
                  done <- downloadModels configStrings

                  if done
                    then return True
                    else do
                      clientGlobals.cgPrecacheCheck .= Constants.csSounds
                      return False
                else do
                  clientGlobals.cgPrecacheCheck .= Constants.csSounds
                  return False
            else
              return False

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        downloadModels :: Vec.Vector B.ByteString -> Quake Bool
        downloadModels configStrings = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          if precacheCheck < Constants.csModels + Constants.maxModels && B.length (configStrings Vec.! precacheCheck) > 0
            then do
              let str = configStrings Vec.! precacheCheck

              (done, continue) <- checkWildcard str
                                    >>= checkPrecacheModelSkin str
                                    >>= checkPrecacheModel str

              if | done -> return True
                 | continue -> downloadModels configStrings
                 | otherwise -> do
                     Just model <- use $ clientGlobals.cgPrecacheModel
                     let pheader = newDMdlT (BL.fromStrict model)

                     ret <- downloadModel pheader

                     if ret
                       then
                         return True
                       else do
                         clientGlobals.cgPrecacheModel .= Nothing
                         clientGlobals.cgPrecacheModelSkin .= 0
                         clientGlobals.cgPrecacheCheck += 1
                         downloadModels configStrings

            else
              return False

        downloadModel :: DMdlT -> Quake Bool
        downloadModel pheader = do
          precacheModelSkin <- use $ clientGlobals.cgPrecacheModelSkin

          if precacheModelSkin - 1 < (pheader^.dmNumSkins)
            then do
              Just model <- use $ clientGlobals.cgPrecacheModel
              let name = B.take (Constants.maxSkinName * (pheader^.dmNumSkins)) (B.drop ((pheader^.dmOfsSkins) + (precacheModelSkin - 1) * Constants.maxSkinName) model)
                  zeroIdx = B.findIndex (== 0) name
                  name' = case zeroIdx of
                            Nothing -> name
                            Just idx -> B.take idx name

              fileExists <- CLParse.checkOrDownloadFile name'
              clientGlobals.cgPrecacheModelSkin += 1

              if fileExists
                then downloadModel pheader
                else return True
            else
              return False

        checkWildcard :: B.ByteString -> Quake (Bool, Bool)
        checkWildcard str = do
          if str `BC.index` 0 == '*' || str `BC.index` 0 == '#'
            then do
              clientGlobals.cgPrecacheCheck += 1
              return (False, True)
            else
              return (False, False)

        checkPrecacheModelSkin :: B.ByteString -> (Bool, Bool) -> Quake (Bool, Bool)
        checkPrecacheModelSkin _ (True, _) = return (True, False)
        checkPrecacheModelSkin _ (_, True) = return (False, True)
        checkPrecacheModelSkin str _ = do
          precacheModelSkin <- use $ clientGlobals.cgPrecacheModelSkin
          
          if precacheModelSkin == 0
            then do
              fileExists <- CLParse.checkOrDownloadFile str
              clientGlobals.cgPrecacheModelSkin .= 1
              return $ if fileExists then (False, False) else (True, False)
            else
              return (False, False)

        -- checking for skins in the model
        checkPrecacheModel :: B.ByteString -> (Bool, Bool) -> Quake (Bool, Bool)
        checkPrecacheModel _ (True, _) = return (True, False)
        checkPrecacheModel _ (_, True) = return (False, True)
        checkPrecacheModel str _ = do
          precacheModel <- use $ clientGlobals.cgPrecacheModel

          if isNothing precacheModel
            then do
              model <- FS.loadFile str
              clientGlobals.cgPrecacheModel .= model
              checkContentsLoaded model
                >>= checkAliasModel model
                >>= checkModelLoaded model
            else
              return (False, False)

        checkContentsLoaded :: Maybe B.ByteString -> Quake (Bool, Bool)
        checkContentsLoaded Nothing = do
          clientGlobals.cgPrecacheModelSkin .= 0
          clientGlobals.cgPrecacheCheck += 1
          return (False, True) -- couldn't load the file
        checkContentsLoaded _ = return (False, False)

        checkAliasModel :: Maybe B.ByteString -> (Bool, Bool) -> Quake (Bool, Bool)
        checkAliasModel _ (True, _) = return (True, False)
        checkAliasModel _ (_, True) = return (False, True)
        checkAliasModel maybeModel _ = do
          let Just model = maybeModel -- pretty sure it is not Nothing
              header = B.take 4 model

          if header /= idAliasHeader
            then do
              -- not an alias model
              clientGlobals.cgPrecacheModel .= Nothing
              clientGlobals.cgPrecacheModelSkin .= 0
              clientGlobals.cgPrecacheCheck += 1
              return (False, True)
            else
              return (False, False)

        checkModelLoaded :: Maybe B.ByteString -> (Bool, Bool) -> Quake (Bool, Bool)
        checkModelLoaded _ (True, _) = return (True, False)
        checkModelLoaded _ (_, True) = return (False, True)
        checkModelLoaded maybeModel _ = do
          let Just model = maybeModel -- pretty sure it is not Nothing
              pheader = newDMdlT (BL.fromStrict model)

          if (pheader^.dmVersion) /= Constants.aliasVersion
            then do
              clientGlobals.cgPrecacheCheck += 1
              clientGlobals.cgPrecacheModelSkin .= 0
              return (False, True) -- couldn't load it
            else
              return (False, False)

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        checkSoundsDownload :: Bool -> Quake Bool
        checkSoundsDownload True = return True -- do nothing
        checkSoundsDownload False = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          if precacheCheck >= Constants.csSounds && precacheCheck < Constants.csSounds + Constants.maxSounds
            then do
              allowDownloadSoundsValue <- liftM (^.cvValue) allowDownloadSoundsCVar

              if allowDownloadSoundsValue /= 0
                then do
                  when (precacheCheck == Constants.csSounds) $
                    clientGlobals.cgPrecacheCheck += 1 -- zero is blank

                  configStrings <- use $ globals.cl.csConfigStrings
                  done <- downloadSounds configStrings

                  if done
                    then
                      return True

                    else do
                      clientGlobals.cgPrecacheCheck .= Constants.csImages
                      return False

                else do
                  clientGlobals.cgPrecacheCheck .= Constants.csImages
                  return False

            else
              return False

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        downloadSounds :: Vec.Vector B.ByteString -> Quake Bool
        downloadSounds configStrings = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          if precacheCheck < Constants.csSounds + Constants.maxSounds && B.length (configStrings Vec.! precacheCheck) > 0
            then do
              let str = configStrings Vec.! precacheCheck
              clientGlobals.cgPrecacheCheck += 1

              if str `BC.index` 0 == '*'
                then
                  downloadSounds configStrings
                else do
                  let fn = "sound/" `B.append` str
                  fileExists <- CLParse.checkOrDownloadFile fn
                  if fileExists
                    then downloadSounds configStrings
                    else return True
            else
              return False

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        checkImagesDownload :: Bool -> Quake Bool
        checkImagesDownload True = return True -- do nothing
        checkImagesDownload False = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          if precacheCheck >= Constants.csImages && precacheCheck < Constants.csImages + Constants.maxImages
            then do
              when (precacheCheck == Constants.csImages) $
                clientGlobals.cgPrecacheCheck += 1 -- zero is blank

              configStrings <- use $ globals.cl.csConfigStrings
              done <- downloadImages configStrings

              if done
                then
                  return True
                else do
                  clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins
                  return False
            else
              return False

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        downloadImages :: Vec.Vector B.ByteString -> Quake Bool
        downloadImages configStrings = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck
          
          if precacheCheck < Constants.csImages + Constants.maxImages && B.length (configStrings Vec.! precacheCheck) > 0
            then do
              let str = configStrings Vec.! precacheCheck
              clientGlobals.cgPrecacheCheck += 1

              let fn = "pics/" `B.append` str `B.append` ".pcx"
              fileExists <- CLParse.checkOrDownloadFile fn
              if fileExists
                then downloadImages configStrings
                else return True
            else
              return False

        -- skins are special, since a player has three things to download:
        -- model, weapon model and skin
        -- so precache_check is now *3
        --
        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        checkSkinsDownload :: Bool -> Quake Bool
        checkSkinsDownload True = return True -- do nothing
        checkSkinsDownload False = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          if precacheCheck >= Constants.csPlayerSkins && precacheCheck < Constants.csPlayerSkins + Constants.maxClients * playerMult
            then do
              allowDownloadPlayersValue <- liftM (^.cvValue) allowDownloadPlayersCVar

              if allowDownloadPlayersValue /= 0
                then do
                  configStrings <- use $ globals.cl.csConfigStrings
                  done <- downloadSkin configStrings

                  if done
                    then
                      return False
                    else do
                      clientGlobals.cgPrecacheCheck .= envCnt
                      return False
                else do
                  -- precache phase completed
                  clientGlobals.cgPrecacheCheck .= envCnt
                  return False
            else
              return False

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        downloadSkin :: Vec.Vector B.ByteString -> Quake Bool
        downloadSkin configStrings = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          if precacheCheck < Constants.csPlayerSkins + Constants.maxClients * playerMult
            then do
              let i = (precacheCheck - Constants.csPlayerSkins) `div` playerMult
                  n = (precacheCheck - Constants.csPlayerSkins) `mod` playerMult
                  str = configStrings Vec.! (Constants.csPlayerSkins + i)

              if B.length str == 0
                then do
                  clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + (i + 1) * playerMult
                  downloadSkin configStrings
                else do
                  let indices = BC.findIndices (== '\\') str
                      (pos, pos2) = case indices of
                                      [] -> (0, fromJust $ BC.findIndex (== '/') str)
                                      [a] -> (a, fromJust $ BC.findIndex (== '/') str)
                                      (a:b:_) -> (a, b)
                      model = B.drop (pos + 1) (B.take pos2 str)
                      skin = B.drop (pos2 + 1) str

                  done <- getModel model n i
                            >>= getWeaponModel model n i
                            >>= getWeaponSkin model n i
                            >>= getSkin model skin n i
                            >>= getSkinI model skin n i

                  if done
                    then
                      return True
                    else do
                      clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + (i + 1) * playerMult
                      downloadSkin configStrings
            else
              return False

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        getModel :: B.ByteString -> Int -> Int -> Quake Bool
        getModel model n i
          | n > 0 = return False
          | otherwise = do
              let fn = "players/" `B.append` model `B.append` "/tris.md2"
              fileExists <- CLParse.checkOrDownloadFile fn
              if fileExists
                then
                  return False
                else do
                  clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + i * playerMult + 1
                  return True

        getWeaponModel :: B.ByteString -> Int -> Int -> Bool -> Quake Bool
        getWeaponModel _ _ _ True = return True
        getWeaponModel model n i _
          | n > 1 = return False
          | otherwise = do
              let fn = "players/" `B.append` model `B.append` "/weapon.md2"
              fileExists <- CLParse.checkOrDownloadFile fn
              if fileExists
                then
                  return False
                else do
                  clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + i * playerMult + 2
                  return True

        getWeaponSkin :: B.ByteString -> Int -> Int -> Bool -> Quake Bool
        getWeaponSkin _ _ _ True = return True
        getWeaponSkin model n i _
          | n > 2 = return False
          | otherwise = do
              let fn = "players/" `B.append` model `B.append` "/weapon.pcx"
              fileExists <- CLParse.checkOrDownloadFile fn
              if fileExists
                then
                  return False
                else do
                  clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + i * playerMult + 3
                  return True

        getSkin :: B.ByteString -> B.ByteString -> Int -> Int -> Bool -> Quake Bool
        getSkin _ _ _ _ True = return True
        getSkin model skin n i _
          | n > 3 = return False
          | otherwise = do
              let fn = "players/" `B.append` model `B.append` "/" `B.append` skin `B.append` ".pcx"
              fileExists <- CLParse.checkOrDownloadFile fn
              if fileExists
                then
                  return False
                else do
                  clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + i * playerMult + 4
                  return True

        getSkinI :: B.ByteString -> B.ByteString -> Int -> Int -> Bool -> Quake Bool
        getSkinI _ _ _ _ True = return True
        getSkinI model skin n i _
          | n > 4 = return False
          | otherwise = do
              let fn = "players/" `B.append` model `B.append` "/" `B.append` skin `B.append` "_i.pcx"
              fileExists <- CLParse.checkOrDownloadFile fn
              if fileExists
                then
                  return False
                else do
                  clientGlobals.cgPrecacheCheck .= Constants.csPlayerSkins + i * playerMult + 5
                  return True

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        loadMap :: Bool -> Quake Bool
        loadMap True = return True -- do nothing
        loadMap False = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          if precacheCheck == envCnt
            then do
              clientGlobals.cgPrecacheCheck += 1
              configStrings <- use $ globals.cl.csConfigStrings

              let str = configStrings Vec.! (Constants.csModels + 1)
                  chk = configStrings Vec.! Constants.csMapChecksum

              (_, checksum) <- CM.loadMap str True [0]
              let mapChecksum = head checksum

              if mapChecksum `xor` Lib.atoi chk /= 0
                then do
                  Com.comError Constants.errDrop $ "Local map version differs from server: " `B.append`
                                                   BC.pack (show mapChecksum) `B.append` " != '" `B.append`
                                                   chk `B.append` "'\n"
                  return True
                else
                  return False
            else
              return False


        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        checkPicsDownload :: Bool -> Quake Bool
        checkPicsDownload True = return True -- do nothing
        checkPicsDownload False = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          if precacheCheck > envCnt && precacheCheck < textureCnt
            then do
              allowDownloadValue <- liftM (^.cvValue) allowDownloadCVar
              allowDownloadMapsValue <- liftM (^.cvValue) allowDownloadMapsCVar

              if allowDownloadValue /= 0 && allowDownloadMapsValue /= 0
                then do
                  configStrings <- use $ globals.cl.csConfigStrings
                  done <- getSkyPic (configStrings Vec.! Constants.csSky)

                  if done
                    then
                      return True
                    else do
                      clientGlobals.cgPrecacheCheck .= textureCnt
                      return False
                else do
                  clientGlobals.cgPrecacheCheck .= textureCnt
                  return False
            else
              return False

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        getSkyPic :: B.ByteString -> Quake Bool
        getSkyPic skyStr = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck
          if precacheCheck < textureCnt
            then do
              clientGlobals.cgPrecacheCheck += 1
              let n = precacheCheck - envCnt - 1
                  fn = "env/" `B.append` skyStr `B.append` (envSuf Vec.! (n `div` 2)) `B.append` (if n .&. 1 /= 0 then ".pcx" else ".tga")

              fileExists <- CLParse.checkOrDownloadFile fn
              if fileExists
                then getSkyPic skyStr
                else return True
            else
              return False

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        updatePrecacheCheck2 :: Bool -> Quake Bool
        updatePrecacheCheck2 True = return True -- do nothing
        updatePrecacheCheck2 False = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          when (precacheCheck == textureCnt) $ do
            clientGlobals.cgPrecacheCheck .= textureCnt + 1
            clientGlobals.cgPrecacheTex .= 0

          return False

        -- confirm existance of textures, download any that don't exist
        --
        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        checkTexturesDownload :: Bool -> Quake Bool
        checkTexturesDownload True = return True -- do nothing
        checkTexturesDownload False = do
          precacheCheck <- use $ clientGlobals.cgPrecacheCheck

          if precacheCheck == textureCnt + 1
            then do
              allowDownloadValue <- liftM (^.cvValue) allowDownloadCVar
              allowDownloadMapsValue <- liftM (^.cvValue) allowDownloadMapsCVar

              if allowDownloadValue /= 0 && allowDownloadMapsValue /= 0
                then do
                  mapSurfaces <- use $ cmGlobals.cmMapSurfaces

                  done <- downloadTextures mapSurfaces

                  if done
                    then
                      return True
                    else do
                      clientGlobals.cgPrecacheCheck .= textureCnt + 999
                      return False
                else do
                  clientGlobals.cgPrecacheCheck .= textureCnt + 999
                  return False
            else
              return False

        -- returns True if we started a download and
        -- need to quit the requestNextDownload function
        downloadTextures :: Vec.Vector MapSurfaceT -> Quake Bool
        downloadTextures mapSurfaces = do
          precacheTex <- use $ clientGlobals.cgPrecacheTex
          numTexInfo <- use $ cmGlobals.cmNumTexInfo

          if precacheTex < numTexInfo
            then do
              let fn = "textures/" `B.append` (fromJust $ (mapSurfaces Vec.! precacheTex)^.msRName) `B.append` ".wal"
              clientGlobals.cgPrecacheTex += 1

              fileExists <- CLParse.checkOrDownloadFile fn
              if fileExists
                then downloadTextures mapSurfaces
                else return True
            else
              return False
        
        finishDownloads :: Bool -> Quake ()
        finishDownloads True = return ()
        finishDownloads False = do
          CLParse.registerSounds
          CLView.prepRefresh

          precacheSpawnCount <- use $ clientGlobals.cgPrecacheSpawnCount

          MSG.writeByteI (globals.cls.csNetChan.ncMessage) Constants.clcStringCmd
          MSG.writeString (globals.cls.csNetChan.ncMessage) ("begin " `B.append` BC.pack (show precacheSpawnCount) `B.append` "\n") -- IMPROVE?
