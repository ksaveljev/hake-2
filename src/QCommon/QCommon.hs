module QCommon.QCommon
  ( frame
  , initialize
  ) where

import qualified Client.CL as CL
import qualified Client.Key as Key
import qualified Client.SCR as SCR
import qualified Constants
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
import           Util.Binary (encode)

import           Control.Lens ((^.))
import           Control.Monad (unless,void)
import           Data.Bits ((.|.))
import qualified Data.ByteString as B

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
     CVar.initializeCVars initialCVars
     NET.initialize
     NetChannel.initialize
     SVMain.initialize
     CL.initialize
     processUserCommands
     Com.printf "====== Quake2 Initialized ======\n\n"
     CL.writeConfiguration
     
reconfigure :: Bool -> Quake ()
reconfigure clear =
  do cdDirCVar <- CVar.get "cddir" "" Constants.cvarArchive
     maybe (Com.fatalError "cddir cvar not set") proceed cdDirCVar
  where proceed cdDir =
          do CBuf.addText "exec default.cfg\n\
                          \bind MWHEELUP weapnext\n\
                          \bind MWHEELDOWN weapprev\n\
                          \bind w +forward\n\
                          \bind s +back\n\
                          \bind a +moveleft\n\
                          \bind d +moveright\n"
             CBuf.execute
             void (CVar.set "vid_fullscreen" "0")
             CBuf.addText "exec config.cfg\n"
             CBuf.addEarlyCommands clear
             CBuf.execute
             unless (B.null (cdDir^.cvString))
               (void (CVar.set "cddir" (cdDir^.cvString)))

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars = [ ("host_speeds", "0", 0)
               , ("log_stats", "0", 0)
               , ("developer", "0", Constants.cvarArchive)
               , ("timescale", "0", 0)
               , ("fixedtime", "0", 0)
               , ("logfile", "0", 0)
               , ("showtrace", "0", 0)
               , ("dedicated", "0", Constants.cvarNoSet)
               , version
               ]
  where version = ("version", v, Constants.cvarServerInfo .|. Constants.cvarNoSet)
        v = B.concat [encode Constants.version, " ", Constants.__date__]

processUserCommands :: Quake ()
processUserCommands =
  do added <- CBuf.addLateCommands -- add + commands from command line
     process added
  where process True = SCR.endLoadingPlaque -- the user asked for something explicit so drop the loading plaque
        process False = runDefault -- the user didn't give any commands, run default action

runDefault :: Quake ()
runDefault =
  do dedicatedValue <- fmap (^.cvValue) dedicatedCVar
     CBuf.addText (if dedicatedValue == 0 then "d1\n" else "dedicated_start\n")
     CBuf.execute
