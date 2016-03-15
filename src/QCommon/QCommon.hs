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
import           QuakeState
import qualified Server.SVMain as SVMain
import qualified Sys.NET as NET
import           Types
import           Util.Binary (encode)
import qualified Util.Lib as Lib
import qualified Sys.Timer as Timer

import           Control.Lens (use, (^.), (.=), (&), (.~))
import           Control.Monad (when, unless, void)
import           Data.Bits ((.|.))
import qualified Data.ByteString as B
import           System.IO (Handle, hSeek, hSetFileSize, SeekMode(AbsoluteSeek), IOMode(WriteMode))

frame :: Int -> Quake ()
frame msec =
  do handleLogStats =<< logStatsCVar
     updatedMsec <- calcMsec msec
     showTrace
     CBuf.execute
     timeBefore <- getCurrentTime
     serverFrame updatedMsec
     timeBetween <- getCurrentTime
     clientFrame updatedMsec
     timeAfter <- getCurrentTime
     printTimeStats timeBefore timeBetween timeAfter
  where getCurrentTime =
          do hsv <- fmap (^.cvValue) hostSpeedsCVar
             if hsv /= 0.0 then Timer.milliseconds else return 0
        serverFrame updatedMsec =
          do comGlobals.cgDebugContext .= "SV:"
             SVMain.frame updatedMsec
        clientFrame updatedMsec =
          do comGlobals.cgDebugContext .= "CL:"
             CL.frame updatedMsec

handleLogStats :: CVarT -> Quake ()
handleLogStats logStats
  | not (logStats^.cvModified) = return ()
  | otherwise =
      do CVar.update (logStats & cvModified .~ False)
         closeLogStatsFile
         when ((logStats^.cvValue) /= 0) $
           openLogStatsFile "stats.log" >>= addLogStatsHeader

openLogStatsFile :: B.ByteString -> Quake (Maybe Handle)
openLogStatsFile name =
  Lib.fOpen name WriteMode >>= maybe (return Nothing) (fmap Just . prepareLogStatsFile)
  
prepareLogStatsFile :: Handle -> Quake Handle
prepareLogStatsFile h = -- IMPROVE: handle exceptions
  do request (io (do hSeek h AbsoluteSeek 0
                     hSetFileSize h 0))
     globals.gLogStatsFile .= Just h
     return h

closeLogStatsFile :: Quake ()
closeLogStatsFile =
  do statsFile <- use (globals.gLogStatsFile)
     maybe (return ()) closeFile statsFile
  where closeFile h =
          do Lib.fClose h
             globals.gLogStatsFile .= Nothing

addLogStatsHeader :: Maybe Handle -> Quake ()
addLogStatsHeader Nothing = return ()
addLogStatsHeader (Just h) =
  request (io (B.hPut h "entities,dlights,parts,frame time\n")) -- IMPROVE: catch exception ?
  
calcMsec :: Int -> Quake Int
calcMsec msec = updateMsec <$> ft <*> ts
  where ft = fmap (^.cvValue) fixedTimeCVar
        ts = fmap (^.cvValue) timeScaleCVar
        updateMsec ftv tsv
          | ftv /= 0 = truncate ftv
          | tsv /= 0 = let tmp = fromIntegral msec * tsv
                       in if tmp < 1.0 then 1 else truncate tmp
          | otherwise = msec

showTrace :: Quake ()
showTrace =
  do stv <- fmap (^.cvValue) showTraceCVar
     when (stv /= 0) $
       do ct <- use (globals.gCTraces)
          cpc <- use (globals.gCPointContents)
          Com.printf (B.concat [encode ct, " traces ", encode cpc, " points\n"])
          globals.gCTraces .= 0
          globals.gCBrushTraces .= 0
          globals.gCPointContents .= 0

printTimeStats :: Int -> Int -> Int -> Quake ()
printTimeStats _ _ _ = -- to avoid unused var warning
-- printTimeStats timeBefore timeBetween timeAfter =
  do hsv <- fmap (^.cvValue) hostSpeedsCVar
     when (hsv /= 0) $
       error "QCommon.printTimeStats" -- TODO
       {- let timeAll = timeAfter - timeBefore
              timeSV = timeBetween - timeBefore
              timeCL = timeAfter - timeBetween
              {-
              int gm= Globals.time_after_game - Globals.time_before_game;
              int rf= Globals.time_after_ref - Globals.time_before_ref;
              sv -= gm;
              cl -= rf; -}
              -}

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
  do cdDirCVar <- CVar.get "cddir" B.empty Constants.cvarArchive
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
processUserCommands = CBuf.addLateCommands >>= process -- add + commands from command line
  where process True = SCR.endLoadingPlaque -- the user asked for something explicit so drop the loading plaque
        process False = runDefault -- the user didn't give any commands, run default action

runDefault :: Quake ()
runDefault = getDefaultCommand >>= CBuf.addText >> CBuf.execute
  where dedicatedValue = fmap (^.cvValue) dedicatedCVar
        getDefaultCommand = fmap defaultCommand dedicatedValue
        defaultCommand v | v == 0 = "d1\n"
                         | otherwise = "dedicated_start\n"