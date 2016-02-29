module Client.CL
  ( frame
  , initialize
  , writeConfiguration
  ) where

import qualified Client.CLInput as CLInput
import qualified Client.CLParse as CLParse
import qualified Client.Console as Console
import qualified Client.Menu as Menu
import qualified Client.SCR as SCR
import qualified Client.V as V
import qualified Client.VID as VID
import qualified Constants
import           Data.Bits ((.|.))
import qualified Game.Cmd as Cmd
import qualified QCommon.CBuf as CBuf
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import qualified QCommon.FS as FS
import qualified QCommon.SZ as SZ
import           QuakeState
import qualified Sound.S as S
import qualified Sys.IN as IN
import qualified Sys.Timer as Timer
import           Types

import           Control.Lens ((^.), (.=), (&), (.~))
import           Control.Monad (unless)
import qualified Data.ByteString as B

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars =
  [ ("adr0", "", Constants.cvarArchive), ("adr1", "", Constants.cvarArchive)
  , ("adr2", "", Constants.cvarArchive), ("adr3", "", Constants.cvarArchive)
  , ("adr4", "", Constants.cvarArchive), ("adr5", "", Constants.cvarArchive)
  , ("adr6", "", Constants.cvarArchive), ("adr7", "", Constants.cvarArchive)
  , ("adr8", "", Constants.cvarArchive)
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
  , ("rcon_password", "", 0) , ("rcon_address", "", 0) , ("r_lightlevel", "0", 0)
  , ("password", "", Constants.cvarUserInfo)
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
          SZ.initialize (globals.gNetMessage) "" Constants.maxMsgLen
          Menu.initialize
          SCR.initialize
          initializeLocal
          IN.initialize
          FS.execAutoexec
          CBuf.execute

writeConfiguration :: Quake ()
writeConfiguration = error "CL.writeConfiguration" -- TODO

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

quitF :: XCommandT
quitF = error "CL.quitF" -- TODO

connectF :: XCommandT
connectF = error "CL.connectF" -- TODO

reconnectF :: XCommandT
reconnectF = error "CL.reconnectF" -- TODO

rconF :: XCommandT
rconF = error "CL.rconF" -- TODO

precacheF :: XCommandT
precacheF = error "CL.precacheF" -- TODO

frame :: Int -> Quake ()
frame = error "CL.frame" -- TODO