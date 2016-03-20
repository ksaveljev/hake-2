module Client.CL
  ( dropClient
  , frame
  , initialize
  , quitF
  , writeConfiguration
  ) where

import           Client.CheatVarT
import qualified Client.CLFX as CLFX
import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.CLInput as CLInput
import qualified Client.CLParse as CLParse
import qualified Client.CLPred as CLPred
import           Client.CLShared as CLShared
import qualified Client.CLView as CLView
import qualified Client.Console as Console
import qualified Client.Key as Key
import qualified Client.Menu as Menu
import           Client.RefDefT
import qualified Client.SCR as SCR
import qualified Client.V as V
import qualified Client.VID as VID
import qualified Constants
import           Data.Bits ((.|.))
import qualified Game.Cmd as Cmd
import           Game.CVarT
import qualified QCommon.CBuf as CBuf
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import qualified QCommon.FS as FS
import qualified QCommon.NetChannel as NetChannel
import           QCommon.NetChanT
import           QCommon.SizeBufT
import qualified QCommon.SZ as SZ
import           QuakeState
import qualified Sound.S as S
import qualified Sys.IN as IN
import qualified Sys.NET as NET
import qualified Sys.Sys as Sys
import qualified Sys.Timer as Timer
import           Types
import qualified Util.Lib as Lib

import           Control.Concurrent (threadDelay)
import           Control.Lens (preuse, use, ix, (^.), (.=), (+=), (&), (.~))
import           Control.Monad (unless, when, void)
import qualified Data.ByteString as B
import qualified Data.Vector as Vector
import           System.IO (Handle, IOMode(ReadWriteMode), hSeek, hSetFileSize, SeekMode(AbsoluteSeek))

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
  [ ("adr0", B.empty, Constants.cvarArchive), ("adr1", B.empty, Constants.cvarArchive)
  , ("adr2", B.empty, Constants.cvarArchive), ("adr3", B.empty, Constants.cvarArchive)
  , ("adr4", B.empty, Constants.cvarArchive), ("adr5", B.empty, Constants.cvarArchive)
  , ("adr6", B.empty, Constants.cvarArchive), ("adr7", B.empty, Constants.cvarArchive)
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
  , ("m_yaw", "0.022", 0) , ("m_forward", "1", 0) , ("m_side", "1", 0)
  , ("cl_shownet", "0", 0) , ("cl_showmiss", "0", 0) , ("showclamp", "0", 0)
  , ("cl_timeout", "120", 0) , ("paused", "0", 0) , ("timedemo", "0", 0)
  , ("rcon_password", B.empty, 0) , ("rcon_address", B.empty, 0) , ("r_lightlevel", "0", 0)
  , ("password", B.empty, Constants.cvarUserInfo)
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
initialize =
  do inDedicatedMode <- fmap ((/= 0) . (^.cvValue)) dedicatedCVar
     unless inDedicatedMode $
       do Console.initialize
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
writeConfiguration =
  do path <- getConfigurationFilePath
     fileHandle <- Lib.fOpen path ReadWriteMode
     maybe fileHandleError (proceedWriteConfiguration path) fileHandle
  where fileHandleError = error "CL.writeConfiguration fileHandle is Nothing"
        proceedWriteConfiguration path fileHandle =
          do request (io (writeConfigFileHeader fileHandle))
             Key.writeBindings fileHandle
             Lib.fClose fileHandle
             CVar.writeVariables path

getConfigurationFilePath :: Quake B.ByteString
getConfigurationFilePath =
  do gameDir <- FS.gameDir
     return (gameDir `B.append` "/config.cfg")

-- IMPROVE: exceptions
writeConfigFileHeader :: Handle -> IO ()
writeConfigFileHeader fileHandle =
  do hSeek fileHandle AbsoluteSeek 0
     hSetFileSize fileHandle 0
     B.hPut fileHandle "// generated by quake, do not modify\n"

initializeLocal :: Quake ()
initializeLocal =
  do initializeClientState
     CLInput.initializeInput
     CVar.initializeCVars initialCVars
     clearGenderCVar
     Cmd.addInitialCommands initialCommands
  where initializeClientState =
          do globals.gCls.csState .= Constants.caDisconnected
             msec <- Timer.milliseconds
             globals.gCls.csRealTime .= msec
        clearGenderCVar =
          do gender <- genderCVar
             CVar.update (gender & cvModified .~ False)

forwardToServerF :: XCommandT
forwardToServerF = error "CL.forwardToServerF" -- TODO

pauseF :: XCommandT
pauseF = error "CL.pauseF" -- TODO

pingServersF :: XCommandT
pingServersF = error "CL.pingServersF" -- TODO

skinsF :: XCommandT
skinsF = error "CL.skinsF" -- TODO

userInfoF :: XCommandT
userInfoF = error "CL.userInfoF" -- TODO

sndRestartF :: XCommandT
sndRestartF = error "CL.sndRestartF" -- TODO

changingF :: XCommandT
changingF = error "CL.changingF" -- TODO

disconnectF :: XCommandT
disconnectF = error "CL.disconnectF" -- TODO

recordF :: XCommandT
recordF = error "CL.recordF" -- TODO

stopF :: XCommandT
stopF = error "CL.stopF" -- TODO

connectF :: XCommandT
connectF = error "CL.connectF" -- TODO

reconnectF :: XCommandT
reconnectF = error "CL.reconnectF" -- TODO

rconF :: XCommandT
rconF = error "CL.rconF" -- TODO

precacheF :: XCommandT
precacheF = error "CL.precacheF" -- TODO

frame :: Int -> Quake ()
frame msec = runFrame msec =<< dedicatedCVar

runFrame :: Int -> CVarT -> Quake ()
runFrame msec dedicated
  | (dedicated^.cvValue) /= 0 = return ()
  | otherwise =
      do clientGlobals.cgExtraTime += msec
         skip <- shouldSkip
         unless skip (clientFrame msec)

shouldSkip :: Quake Bool
shouldSkip =
  do timeDemo <- timeDemoCVar
     maxFps <- clMaxFPSCVar
     state <- use (globals.gCls.csState)
     extraTime <- use (clientGlobals.cgExtraTime)
     checkSkipConditions timeDemo maxFps state extraTime
  where checkSkipConditions timeDemo maxFps state extraTime
          | (timeDemo^.cvValue) == 0 && ((state == Constants.caConnected && extraTime < 100) || (fromIntegral extraTime < 1000 / (maxFps^.cvValue))) =
              return True
          | otherwise = return False

clientFrame :: Int -> Quake ()
clientFrame msec =
  do IN.frame
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
calcSimulationTime =
  do extraTime <- use (clientGlobals.cgExtraTime)
     curTime <- use (globals.gCurTime)
     globals.gCls.csFrameTime .= max (fromIntegral extraTime / 1000) 0.2
     globals.gCls.csRealTime .= curTime
     globals.gCl.csTime += extraTime
     clientGlobals.cgExtraTime .= 0

checkDebugger :: Int -> Quake ()
checkDebugger msec
  | msec > 5000 =
      do millis <- Timer.milliseconds
         globals.gCls.csNetChan.ncLastReceived .= millis
  | otherwise = return ()

renderingDLLChange :: Quake ()
renderingDLLChange =
  do VID.checkChanges
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
updateAudio =
  do cl <- use (globals.gCl)
     S.update (cl^.csRefDef.rdViewOrg) (cl^.csVForward) (cl^.csVRight) (cl^.csVUp)

checkKeyDest :: Quake ()
checkKeyDest =
  do keyDest <- use (globals.gCls.csKeyDest)
     state <- use (globals.gCls.csState)
     when (state /= Constants.caActive || keyDest /= Constants.keyGame) $
       request (io (threadDelay (20 * 1000)))

dropClient :: Quake ()
dropClient =
  do clientStatic <- use (globals.gCls)
     when ((clientStatic^.csState) `notElem` [Constants.caUninitialized, Constants.caDisconnected])
       disconnect
     when ((clientStatic^.csDisableServerCount) /= -1)
       SCR.endLoadingPlaque

disconnect :: Quake ()
disconnect = error "CL.disconnect" -- TODO

sendCommand :: Quake ()
sendCommand =
  do Sys.sendKeyEvents
     IN.commands
     CBuf.execute
     fixCVarCheats
     CLInput.sendCmd
     checkForResend

readPackets :: Quake ()
readPackets =
  do nextPacket
     checkTimeout

checkTimeout :: Quake ()
checkTimeout =
  do cls <- use (globals.gCls)
     timeout <- timeoutCVar
     doCheckTimeout cls timeout
  where doCheckTimeout cls timeout
          | (cls^.csState) >= Constants.caConnected && (cls^.csRealTime) - (cls^.csNetChan.ncLastReceived) > (truncate (timeout^.cvValue)) * 1000 =
              do globals.gCl.csTimeOutCount += 1
                 serverTimeout =<< use (globals.gCl.csTimeOutCount)
          | otherwise = globals.gCl.csTimeOutCount .= 0
        serverTimeout count
          | count > 5 =
              do Com.printf "\nServer connection timed out.\n"
                 disconnect
          | otherwise = return ()

nextPacket :: Quake ()
nextPacket =
  do gotSomething <- NET.getPacket Constants.nsClient (globals.gNetFrom) (globals.gNetMessage)
     when gotSomething $
       do cls <- use (globals.gCls)
          netMsg <- use (globals.gNetMessage)
          processPacket cls netMsg
          nextPacket

processPacket :: ClientStaticT -> SizeBufT -> Quake ()
processPacket cls netMsg
  | B.take 4 (netMsg^.sbData) == remoteCommandHeader = connectionlessPacket
  | (cls^.csState) `elem` [Constants.caDisconnected, Constants.caConnecting] = return ()
  | (netMsg^.sbCurSize) < 8 =
      do from <- use (globals.gNetFrom)
         Com.printf ((NET.adrToString from) `B.append` ": Runt packet\n")
  | otherwise =
      do from <- use (globals.gNetFrom)
         remote <- use (globals.gCls.csNetChan.ncRemoteAddress)
         checkSame from remote
  where checkSame from remote
          | not (NET.compareAdr from remote) =
              Com.dprintf ((NET.adrToString from) `B.append` ":sequenced packet without connection\n")
          | otherwise =
              do ok <- NetChannel.process (globals.gCls.csNetChan) (globals.gNetMessage)
                 when ok CLParse.parseServerMessage

connectionlessPacket :: Quake ()
connectionlessPacket = error "CL.connectionlessPacket" -- TODO

fixCVarCheats :: Quake ()
fixCVarCheats =
  do maxClients <- preuse (globals.gCl.csConfigStrings.ix Constants.csMaxClients)
     maybe maxClientsError checkSingleOrMultiplayer maxClients
  where maxClientsError = error "CL.fixCVarCheats maxClients is Nothing"
        checkSingleOrMultiplayer maxClients
          | maxClients == "1" || B.null maxClients = return () -- single player can cheat
          | otherwise = Vector.mapM_ fixCVar cheatVars

fixCVar :: CheatVarT -> Quake ()
fixCVar cheatVar =
  do var <- CVar.getExisting (cheatVar^.chvName)
     when ((var^.cvString) /= (cheatVar^.chvValue)) $
       void (CVar.set (cheatVar^.chvName) (cheatVar^.chvValue))

checkForResend :: Quake ()
checkForResend = error "CL.checkForResend" -- TODO
