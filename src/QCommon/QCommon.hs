{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module QCommon.QCommon where

import Data.Bits ((.|.))
import Control.Lens (use, (.=), (^.))
import Control.Monad (when, liftM, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
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

init :: [String] -> Quake ()
init args = do
    Com.initArgv args >> CBuf.init >> Cmd.init >> CVar.init >> Key.init

    -- we need to add the early commands twice, because
    -- a basedir or cddir needs to be set before execing
    -- config files, but we want other parms to override
    -- the settings of the config files
    CBuf.addEarlyCommands False >> CBuf.execute

    -- if (globals.dedicated.cvValue != 1.0)
    whenQ (liftM (/= 1.0) (use $ cvarGlobals.dedicated.cvValue)) $ do
      undefined -- TODO: Jake2.Q2Dialog.setStatus("initializing filesystem...");

    FS.initFileSystem

    -- if (globals.dedicated.cvValue != 1.0)
    whenQ (liftM (/= 1.0) (use $ cvarGlobals.dedicated.cvValue)) $ do
      undefined -- TODO: Jake2.Q2Dialog.setStatus("loading config...");

    reconfigure False

    FS.setCDDir -- use cddir from config.cfg
    FS.markBaseSearchPaths -- mark the default search paths

    -- if (globals.dedicated.cvValue != 1.0)
    whenQ (liftM (/= 1.0) (use $ cvarGlobals.dedicated.cvValue)) $ do
      undefined -- TODO: Jake2.Q2Dialog.testQ2Data(); // test for valid baseq2

    --
    -- init commands and vars
    --
    Cmd.addCommand "error" (Just Com.errorF)

    void $ CVar.getAndSet "host_speeds" "0" 0 (cvarGlobals.hostSpeeds)
    void $ CVar.getAndSet "log_stats" "0" 0 (cvarGlobals.logStats)
    void $ CVar.getAndSet "developer" "0" Constants.cvarArchive (cvarGlobals.developer)
    void $ CVar.getAndSet "timescale" "0" 0 (cvarGlobals.timeScale)
    void $ CVar.getAndSet "fixedtime" "0" 0 (cvarGlobals.fixedTime)
    void $ CVar.getAndSet "logfile" "0" 0 (cvarGlobals.logfileActive)
    void $ CVar.getAndSet "showtrace" "0" 0 (cvarGlobals.showTrace)
    void $ CVar.getAndSet "dedicated" "0" Constants.cvarNoSet (cvarGlobals.dedicated)

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
    let s = BC.pack $ (show $ Constants.version) ++ (show $ Constants.__date__) -- IMPROVE: use formatting library?

    void $ CVar.get "version" s (Constants.cvarServerInfo .|. Constants.cvarNoSet)

    -- if (globals.dedicated.cvValue != 1.0)
    whenQ (liftM (/= 1.0) (use $ cvarGlobals.dedicated.cvValue)) $ do
      undefined -- TODO: Jake2.Q2Dialog.setStatus("initializing network subsystem...");

    NET.init >> NetChannel.init

    -- if (globals.dedicated.cvValue != 1.0)
    whenQ (liftM (/= 1.0) (use $ cvarGlobals.dedicated.cvValue)) $ do
      undefined -- TODO: Jake2.Q2Dialog.setStatus("initializing server subsystem...");

    SVMain.init

    -- if (globals.dedicated.cvValue != 1.0)
    whenQ (liftM (/= 1.0) (use $ cvarGlobals.dedicated.cvValue)) $ do
      undefined -- TODO: Jake2.Q2Dialog.setStatus("initializing client subsystem...");

    CL.init

    added <- CBuf.addLateCommands

    if added
      then do
        dedicatedValue <- use $ cvarGlobals.dedicated.cvValue

        if dedicatedValue == 0
          then CBuf.addText "d1\n"
          else CBuf.addText "dedicated_start\n"

        CBuf.execute
      else SCR.endLoadingPlaque

    Com.printf "====== Quake2 Initialized ======\n\n"

    CL.writeConfiguration

    -- if (globals.dedicated.cvValue != 1.0)
    whenQ (liftM (/= 1.0) (use $ cvarGlobals.dedicated.cvValue)) $ do
      undefined -- TODO: Jake2.Q2Dialog.dispose();

frame :: Int -> Quake ()
frame msec = do
    whenQ (use $ cvarGlobals.logStats.cvModified) $ do
      cvarGlobals.logStats.cvModified .= False

      lsv <- use $ cvarGlobals.logStats.cvValue

      if lsv /= 0.0
        then undefined -- TODO
        else undefined -- TODO

    ftv <- use $ cvarGlobals.fixedTime.cvValue
    tsv <- use $ cvarGlobals.timeScale.cvValue

    let updatedMsec = if | ftv /= 0.0 -> truncate ftv
                         | tsv /= 0.0 -> let tmp = fromIntegral msec * tsv
                                       in if tmp < 1.0 then 1 else truncate tmp
                         | otherwise -> msec

    stv <- use $ cvarGlobals.showTrace.cvValue

    when (stv /= 0.0) $ do
      ct <- use $ globals.cTraces
      cpc <- use $ globals.cPointContents

      Com.printf $ BC.pack (show ct)
        `B.append` " traces "
        `B.append` BC.pack (show cpc)
        `B.append` " points\n" -- TODO: use binary to convert int to bytestring? OR printf with Text and Text.printf ?

      globals.cTraces .= 0
      globals.cBrushTraces .= 0
      globals.cPointContents .= 0

    CBuf.execute

    hsv <- use $ cvarGlobals.hostSpeeds.cvValue

    timeBefore <- if hsv /= 0.0 then Timer.milliseconds else return 0

    -- Com.debugContext = "SV:" -- TODO
    -- SV_MAIN.SV_FRAME(msec) -- TODO

    timeBetween <- if hsv /= 0.0 then Timer.milliseconds else return 0

    -- Com.debugContext = "CL:" -- TODO
    -- CV.Frame(msec) -- TODO

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

      Com.printf undefined -- TODO

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
