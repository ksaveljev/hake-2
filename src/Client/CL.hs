{-# LANGUAGE OverloadedStrings #-}
module Client.CL where

import Control.Lens (use, (.=), (^.))
import Control.Monad (unless, liftM, when, void)
import Data.Bits ((.|.))
import System.IO (IOMode(ReadWriteMode), hSeek, hSetFileSize, SeekMode(AbsoluteSeek))
import qualified Data.ByteString as B

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import qualified Constants
import qualified Client.CLInput as CLInput
import qualified Client.CLParse as CLParse
import qualified Client.Console as Console
import qualified Client.Key as Key
import qualified Client.Menu as Menu
import qualified Client.SCR as SCR
import qualified Client.V as V
import qualified Client.VID as VID
import {-# SOURCE #-} qualified Game.Cmd as Cmd
import qualified Sound.S as S
import qualified Sys.IN as IN
import qualified QCommon.CBuf as CBuf
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.FS as FS
import qualified Sys.Timer as Timer
import qualified Util.Lib as Lib

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
frame _ = do
    dedicatedValue <- liftM (^.cvValue) dedicatedCVar

    unless (dedicatedValue /= 0) $ do
      io (putStrLn "CL.frame") >> undefined -- TODO

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
