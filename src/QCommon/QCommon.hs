module QCommon.QCommon
  (frame
  ,initialize)
  where

import qualified Client.CL as CL
import qualified Client.Key as Key
import qualified Game.Cmd as Cmd
import qualified QCommon.CBuf as CBuf
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.FS as FS
import qualified QCommon.NetChannel as NetChannel
import qualified Server.SVMain as SVMain
import qualified Sys.NET as NET
import           Types

frame :: Int -> Quake ()
frame = undefined -- TODO

initialize :: [String] -> Quake ()
initialize args =
  do Com.initializeArgv args
     CBuf.initialize
     Cmd.initialize
     CVar.initialize
     Key.initialize
     -- we need to add the early commands twice, because
     -- a basedir or cddir needs to be set before execing
     -- config files, but we want other params to override
     -- the settings of the config files
     CBuf.addEarlyCommands False
     CBuf.execute
     FS.initializeFileSystem
     reconfigure False
     FS.setCDDir -- use cddir from config.cfg
     FS.markBaseSearchPaths -- mark the default search paths
     initializeCommandsAndVars
     NET.initialize
     NetChannel.initialize
     SVMain.initialize
     CL.initialize
     undefined -- TODO

initializeCommandsAndVars :: Quake ()
initializeCommandsAndVars = undefined -- TODO

reconfigure :: Bool -> Quake ()
reconfigure = undefined -- TODO
