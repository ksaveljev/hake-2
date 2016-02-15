module QCommon.QCommon
  (frame
  ,initialize)
  where

import qualified Client.CL as CL
import qualified Client.Key as Key
import qualified Client.SCR as SCR
import qualified Game.Cmd as Cmd
import           Game.CVarT
import qualified QCommon.CBuf as CBuf
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import qualified QCommon.FS as FS
import qualified QCommon.NetChannel as NetChannel
import qualified Server.SVMain as SVMain
import qualified Sys.NET as NET
import           Types

import           Control.Lens ((^.))

frame :: Int -> Quake ()
frame = error "QCommon.frame" -- TODO

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
     processUserCommands
     Com.printf "====== Quake2 Initialized ======\n\n"
     CL.writeConfiguration
     
reconfigure :: Bool -> Quake ()
reconfigure = error "QCommon.reconfigure" -- TODO

initializeCommandsAndVars :: Quake ()
initializeCommandsAndVars = error "QCommon.initializeCommandsAndVars" -- TODO

processUserCommands :: Quake ()
processUserCommands =
  do added <- CBuf.addLateCommands -- add + commands from command line
     if added
       -- the user asked for something explicit so drop the loading plaque
       then SCR.endLoadingPlaque
       -- the user didn't give any commands, run default action
       else runDefault

runDefault :: Quake ()
runDefault =
  do dedicatedValue <- fmap (^.cvValue) dedicatedCVar
     CBuf.addText (if dedicatedValue == 0 then "d1\n" else "dedicated_start\n")
     CBuf.execute