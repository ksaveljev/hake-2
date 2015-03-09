{-# LANGUAGE OverloadedStrings #-}
module QCommon.QCommon where

import Control.Lens

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
    whenQ (use $ globals.logStats.cvModified) $ do
      undefined

    undefined -- TODO

reconfigure :: Bool -> Quake ()
reconfigure clear = do
    Just cddirCVar <- CVar.get "cddir" "" Constants.cvarArchive

    let dir = cddirCVar^.cvString

    undefined -- TODO
