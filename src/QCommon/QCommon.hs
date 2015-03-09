{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module QCommon.QCommon where

import Control.Lens (use, (.=), (^.))
import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.Com as Com
import qualified QCommon.CBuf as CBuf
import qualified QCommon.CVar as CVar
import qualified QCommon.FS as FS
import qualified Game.Cmd as Cmd
import qualified Client.Key as Key

init :: [String] -> Quake ()
init args = do
    Com.initArgv args

    CBuf.init

    Cmd.init
    CVar.init

    Key.init

    CBuf.addEarlyCommands False
    CBuf.execute

    --whenQ () $ do undefined -- TODO

    FS.initFileSystem

    reconfigure False

    undefined -- TODO: many more commands

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

      Com.printf $ (BC.pack $ show ct)
        `B.append` " traces "
        `B.append` (BC.pack $ show cpc)
        `B.append` " points\n" -- TODO: use binary to convert int to bytestring? OR printf with Text and Text.printf ?

      globals.cTraces .= 0
      globals.cBrushTraces .= 0
      globals.cPointContents .= 0

    CBuf.execute

    undefined -- TODO

reconfigure :: Bool -> Quake ()
reconfigure clear = do
    Just cddirCVar <- CVar.get "cddir" "" Constants.cvarArchive

    let dir = cddirCVar^.cvString

    undefined -- TODO
