{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Client.CL where

import Control.Concurrent (threadDelay)
import Control.Lens (use, (.=), (^.), (+=), preuse, ix)
import Control.Monad (unless, liftM, when, void)
import Data.Bits ((.|.))
import System.IO (IOMode(ReadWriteMode), hSeek, hSetFileSize, SeekMode(AbsoluteSeek))
import System.Mem (performGC)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as Vec

import Quake
import QuakeState
import CVarVariables
import Client.CheatVarT
import QCommon.XCommandT
import qualified Constants
import qualified Client.CLFX as CLFX
import {-# SOURCE #-} qualified Client.CLInput as CLInput
import {-# SOURCE #-} qualified Client.CLParse as CLParse
import qualified Client.CLPred as CLPred
import qualified Client.CLView as CLView
import qualified Client.Console as Console
import qualified Client.Key as Key
import qualified Client.Menu as Menu
import qualified Client.SCR as SCR
import qualified Client.V as V
import qualified Client.VID as VID
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified QCommon.CBuf as CBuf
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.FS as FS
import qualified QCommon.MSG as MSG
import qualified QCommon.NetChannel as NetChannel
import qualified Sound.S as S
import qualified Sys.IN as IN
import qualified Sys.NET as NET
import qualified Sys.Sys as Sys
import qualified Sys.Timer as Timer
import qualified Util.Lib as Lib

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
disconnect = io (putStrLn "CL.disconnect") >> undefined -- TODO

forwardToServerF :: XCommandT
forwardToServerF = io (putStrLn "CL.forwardToServerF") >> undefined -- TODO

pauseF :: XCommandT
pauseF = io (putStrLn "CL.pauseF") >> undefined -- TODO

pingServersF :: XCommandT
pingServersF = io (putStrLn "CL.pingServersF") >> undefined -- TODO

skinsF :: XCommandT
skinsF = io (putStrLn "CL.skinsF") >> undefined -- TODO

userInfoF :: XCommandT
userInfoF = io (putStrLn "CL.userInfoF") >> undefined -- TODO

sndRestartF :: XCommandT
sndRestartF = io (putStrLn "CL.sndRestartF") >> undefined -- TODO

changingF :: XCommandT
changingF = io (putStrLn "CL.changingF") >> undefined -- TODO

disconnectF :: XCommandT
disconnectF = io (putStrLn "CL.disconnectF") >> undefined -- TODO

recordF :: XCommandT
recordF = io (putStrLn "CL.recordF") >> undefined -- TODO

stopF :: XCommandT
stopF = io (putStrLn "CL.stopF") >> undefined -- TODO

quitF :: XCommandT
quitF = io (putStrLn "CL.quitF") >> undefined -- TODO

connectF :: XCommandT
connectF = io (putStrLn "CL.connectF") >> undefined -- TODO

reconnectF :: XCommandT
reconnectF = io (putStrLn "CL.reconnectF") >> undefined -- TODO

rconF :: XCommandT
rconF = io (putStrLn "CL.rconF") >> undefined -- TODO

precacheF :: XCommandT
precacheF = io (putStrLn "CL.precacheF") >> undefined -- TODO

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
clearState = io (putStrLn "CL.clearState") >> undefined -- TODO
