{-# LANGUAGE OverloadedStrings #-}
module QCommon.QCommon where

import Control.Lens
import Control.Monad (liftM)

import Quake
import Constants (cvarArchive)
import Game.CVarT
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

    --whenQ (liftM () get) $ do undefined -- TODO

    FS.initFileSystem

    reconfigure False

    undefined -- TODO: many more commands

frame :: Int -> Quake ()
frame = undefined -- TODO

reconfigure :: Bool -> Quake ()
reconfigure clear = do
    dir <- liftM (^.cvString) $ CVar.get "cddir" "" cvarArchive

    undefined -- TODO
