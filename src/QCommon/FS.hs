{-# LANGUAGE OverloadedStrings #-}
module QCommon.FS where

import Data.Bits ((.|.))
import Control.Lens ((^.))
import Control.Monad (when)
import System.Directory (getHomeDirectory)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import Internal
import QuakeState
import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.CVar as CVar

initFileSystem :: Quake ()
initFileSystem = do
    Cmd.addCommand "path" pathF
    Cmd.addCommand "link" linkF
    Cmd.addCommand "dir" dirF

    homeDir <- io getHomeDirectory
    let fsUserDir = BC.pack homeDir `B.append` "/.hake2"

    createPath $ fsUserDir `B.append` "/"
    addGameDirectory fsUserDir

    -- basedir <path>
    -- allows the game to run from outside the data tree
    Just fsBaseDir <- CVar.get "basedir" "." Constants.cvarNoSet

    -- cddir <path>
    -- Logically concatenates the cddir after the basedir for
    -- allows the game to run from outside the data tree
    setCDDir

    -- start up with baseq2 by default
    addGameDirectory $ (fsBaseDir^.cvString) `B.append` "/" `B.append` Constants.baseDirName

    makeBaseSearchPaths

    Just fsGameDirVar <- CVar.get "game" "" (Constants.cvarLatch .|. Constants.cvarServerInfo)

    when (B.length (fsGameDirVar^.cvString) > 0) $ setGameDir (fsGameDirVar^.cvString)

createPath :: B.ByteString -> Quake ()
createPath = undefined -- TODO

addGameDirectory :: B.ByteString -> Quake ()
addGameDirectory = undefined -- TODO

setCDDir :: Quake ()
setCDDir = undefined -- TODO

makeBaseSearchPaths :: Quake ()
makeBaseSearchPaths = undefined -- TODO

setGameDir :: B.ByteString -> Quake ()
setGameDir = undefined -- TODO

pathF :: XCommandT
pathF = undefined -- TODO

linkF :: XCommandT
linkF = undefined -- TODO

dirF :: XCommandT
dirF = undefined -- TODO
