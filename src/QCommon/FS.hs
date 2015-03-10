{-# LANGUAGE OverloadedStrings #-}
module QCommon.FS where

import Data.Bits ((.|.))
import Control.Lens ((^.))
import Control.Monad (when)
import Control.Exception
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import Internal
import QuakeState
import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
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
createPath path = do
    -- TODO: int index = path.lastIndexOf('/')
    --       // -1 if not found and 0 means write to root
    --       if (index > 0) ... then we create a directory

    done <- io (catchAny (createDirectoryIfMissing True (BC.unpack path) >> return (Right ())) $ \_ -> do
      return $ Left ()) -- TODO: maybe somehow include exception message?

    case done of
      Left _ -> Com.printf $ "can't create path \"" `B.append` path `B.append` "\"\n"
      Right _ -> return ()

  where catchAny :: IO a -> (SomeException -> IO a) -> IO a
        catchAny = Control.Exception.catch

-- AddGameDirectory
--
-- Sets fs_gamedir, adds the directory to the head of the path, then loads
-- and adds pak1.pak pak2.pak ...
addGameDirectory :: B.ByteString -> Quake ()
addGameDirectory dir = undefined -- TODO

-- set baseq2 directory
setCDDir :: Quake ()
setCDDir = do
    Just fsCDDir <- CVar.get "cddir" "" Constants.cvarArchive
    when (B.length (fsCDDir^.cvString) > 0) $ addGameDirectory (fsCDDir^.cvString)

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
