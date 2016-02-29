module Server.SVMain
  ( dropClient
  , frame
  , initialize
  , shutdown
  ) where

import qualified Constants
import qualified QCommon.CVar as CVar
import qualified QCommon.SZ as SZ
import           QuakeState
import           Server.SVMainShared
import qualified Server.SVConsoleCommands as SVConsoleCommands
import           Types
import           Util.Binary (encode)

import           Data.Bits ((.|.))
import qualified Data.ByteString as B

initialize :: Quake ()
initialize =
  do SVConsoleCommands.initOperatorCommands
     CVar.initializeCVars initialCVars
     SZ.initialize (globals.gNetMessage) "" Constants.maxMsgLen

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars =
  [ ("rcon_password", "", 0)
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
frame = error "SVMain.frame" -- TODO