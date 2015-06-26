{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module QCommon.QCommon where

import Data.Bits ((.|.))
import Control.Lens (use, (.=), (^.))
import Control.Monad (when, liftM, void)
import Control.Exception (IOException, handle)
import System.IO (Handle, hSeek, hSetFileSize, SeekMode(AbsoluteSeek), IOMode(WriteMode), openFile)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Game.Cmd as Cmd
import qualified Client.Key as Key
import qualified QCommon.FS as FS
import qualified QCommon.Com as Com
import qualified QCommon.CBuf as CBuf
import qualified QCommon.CVar as CVar
import qualified QCommon.NetChannel as NetChannel
import qualified Client.CL as CL
import qualified Client.SCR as SCR
import qualified Server.SVMain as SVMain
import qualified Sys.NET as NET
import qualified Sys.Timer as Timer
import qualified Util.Lib as Lib

init :: [String] -> Quake ()
init args = do
    Com.initArgv args >> CBuf.init >> Cmd.init >> CVar.init >> Key.init

    -- we need to add the early commands twice, because
    -- a basedir or cddir needs to be set before execing
    -- config files, but we want other parms to override
    -- the settings of the config files
    CBuf.addEarlyCommands False >> CBuf.execute

    -- if (globals.dedicated.cvValue != 1.0)
    {-
    whenQ (liftM ((/= 1.0) . (^.cvValue)) dedicatedCVar) $ do
      undefined -- TODO: Jake2.Q2Dialog.setStatus("initializing filesystem...");
    -}

    FS.initFileSystem

    -- if (globals.dedicated.cvValue != 1.0)
    {-
    whenQ (liftM ((/= 1.0) . (^.cvValue)) dedicatedCVar) $ do
      undefined -- TODO: Jake2.Q2Dialog.setStatus("loading config...");
    -}

    reconfigure False

    FS.setCDDir -- use cddir from config.cfg
    FS.markBaseSearchPaths -- mark the default search paths

    -- if (globals.dedicated.cvValue != 1.0)
    {-
    whenQ (liftM ((/= 1.0) . (^.cvValue)) dedicatedCVar) $ do
      undefined -- TODO: Jake2.Q2Dialog.testQ2Data(); // test for valid baseq2
    -}

    --
    -- init commands and vars
    --
    Cmd.addCommand "error" (Just Com.errorF)

    void $ CVar.get "host_speeds" "0" 0
    void $ CVar.get "log_stats" "0" 0
    void $ CVar.get "developer" "0" Constants.cvarArchive
    void $ CVar.get "timescale" "0" 0
    void $ CVar.get "fixedtime" "0" 0
    void $ CVar.get "logfile" "0" 0
    void $ CVar.get "showtrace" "0" 0
    void $ CVar.get "dedicated" "0" Constants.cvarNoSet

    {- IMPROVE:
       public static final String BUILDSTRING = "Java " + System.getProperty("java.version");;
       public static final String CPUSTRING = System.getProperty("os.arch");
       String s = Com.sprintf("%4.2f %s %s %s",
                       new Vargs(4)
                               .add(Globals.VERSION)
                               .add(CPUSTRING)
                               .add(Globals.__DATE__)
                               .add(BUILDSTRING));
    -}
    let s = BC.pack (show Constants.version) `B.append` " " `B.append` Constants.__date__ -- IMPROVE: use formatting library?

    void $ CVar.get "version" s (Constants.cvarServerInfo .|. Constants.cvarNoSet)

    -- if (globals.dedicated.cvValue != 1.0)
    {-
    whenQ (liftM ((/= 1.0) . (^.cvValue)) dedicatedCVar) $ do
      undefined -- TODO: Jake2.Q2Dialog.setStatus("initializing network subsystem...");
    -}

    NET.init >> NetChannel.init

    -- if (globals.dedicated.cvValue != 1.0)
    {-
    whenQ (liftM ((/= 1.0) . (^.cvValue)) dedicatedCVar) $ do
      undefined -- TODO: Jake2.Q2Dialog.setStatus("initializing server subsystem...");
    -}

    SVMain.init

    -- if (globals.dedicated.cvValue != 1.0)
    {-
    whenQ (liftM ((/= 1.0) . (^.cvValue)) dedicatedCVar) $ do
      undefined -- TODO: Jake2.Q2Dialog.setStatus("initializing client subsystem...");
    -}

    CL.init

    added <- CBuf.addLateCommands

    if not added
      then do
        dedicatedValue <- liftM (^.cvValue) dedicatedCVar

        if dedicatedValue == 0
          then CBuf.addText "d1\n"
          else CBuf.addText "dedicated_start\n"

        CBuf.execute
      else SCR.endLoadingPlaque

    Com.printf "====== Quake2 Initialized ======\n\n"

    CL.writeConfiguration

    -- if (globals.dedicated.cvValue != 1.0)
    {-
    whenQ (liftM ((/= 1.0) . (^.cvValue)) dedicatedCVar) $ do
      undefined -- TODO: Jake2.Q2Dialog.dispose();
    -}

{-
- Trigger generation of a frame for the given time. The setjmp/longjmp
- mechanism of the original was replaced with exceptions.
- @param msec the current game time
-}
frame :: Int -> Quake ()
frame msec = do
    logStats <- logStatsCVar
    when (logStats^.cvModified) $ do
      CVar.update logStats { _cvModified = False }

      let lsv = logStats^.cvValue

      if lsv /= 0.0
        then do
          closeLogStatsFile
          openedFile <- openLogStatsFile "stats.log"
          case openedFile of
            Nothing -> return ()
            Just h -> io $ B.hPut h "entities,dlights,parts,frame time\n" -- IMPROVE: catch exception ?
        else closeLogStatsFile

    ftv <- liftM (^.cvValue) fixedTimeCVar
    tsv <- liftM (^.cvValue) timeScaleCVar

    let updatedMsec = if | ftv /= 0.0 -> truncate ftv
                         | tsv /= 0.0 -> let tmp = fromIntegral msec * tsv
                                       in if tmp < 1.0 then 1 else truncate tmp
                         | otherwise -> msec

    stv <- liftM (^.cvValue) showTraceCVar

    when (stv /= 0.0) $ do
      ct <- use $ globals.cTraces
      cpc <- use $ globals.cPointContents

      Com.printf $ BC.pack (show ct)
        `B.append` " traces "
        `B.append` BC.pack (show cpc)
        `B.append` " points\n" -- IMPROVE: use binary to convert int to bytestring? OR printf with Text and Text.printf ?

      globals.cTraces .= 0
      globals.cBrushTraces .= 0
      globals.cPointContents .= 0

    CBuf.execute

    hsv <- liftM (^.cvValue) hostSpeedsCVar

    timeBefore <- if hsv /= 0.0 then Timer.milliseconds else return 0

    comGlobals.cgDebugContext .= "SV:"
    SVMain.frame msec

    timeBetween <- if hsv /= 0.0 then Timer.milliseconds else return 0

    comGlobals.cgDebugContext .= "CL:"
    CL.frame msec

    when (hsv /= 0) $ do
      timeAfter <- Timer.milliseconds
      let timeAll = timeAfter - timeBefore
          timeSV = timeBetween - timeBefore
          timeCL = timeAfter - timeBetween
          {- TODO
          int gm= Globals.time_after_game - Globals.time_before_game;
          int rf= Globals.time_after_ref - Globals.time_before_ref;
          sv -= gm;
          cl -= rf;
          -}

      io (putStrLn "QCommon.frame") >> undefined -- TODO

  where closeLogStatsFile :: Quake ()
        closeLogStatsFile = do
          statsFile <- use $ globals.logStatsFile
          case statsFile of
            Nothing -> return ()
            Just h -> do
              Lib.fClose h
              globals.logStatsFile .= Nothing

        openLogStatsFile :: B.ByteString -> Quake (Maybe Handle)
        openLogStatsFile name = do
          -- IMPROVE: set to Nothing in case of exception ?
          io (handle (\(_ :: IOException) -> return Nothing) $ do
                h <- openFile (BC.unpack name) WriteMode
                hSeek h AbsoluteSeek 0
                hSetFileSize h 0
                return $ Just h
             )

reconfigure :: Bool -> Quake ()
reconfigure clear = do
    Just cddirCVar <- CVar.get "cddir" "" Constants.cvarArchive

    let dir = cddirCVar^.cvString

    CBuf.addText "exec default.cfg\n"
    CBuf.addText "bind MWHEELUP weapnext\n"
    CBuf.addText "bind MWHEELDOWN weapprev\n"
    CBuf.addText "bind w +forward\n"
    CBuf.addText "bind s +back\n"
    CBuf.addText "bind a +moveleft\n"
    CBuf.addText "bind d +moveright\n"

    CBuf.execute

    void $ CVar.set "vid_fullscreen" "0"
    CBuf.addText "exec config.cfg\n"

    CBuf.addEarlyCommands clear
    CBuf.execute

    when (dir /= "") $ void (CVar.set "cddir" dir)
