{-# LANGUAGE OverloadedStrings #-}
module QCommon.FS where

import Data.Bits ((.|.))
import Data.Maybe (isJust)
import Control.Lens ((^.), (.=), (%=), use)
import Control.Monad (when, void)
import Control.Exception
import System.Directory
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
    let initialFsUserDir = BC.pack homeDir `B.append` "/.hake2"
    fsGlobals.fsUserDir .= initialFsUserDir

    createPath $ initialFsUserDir `B.append` "/"
    addGameDirectory initialFsUserDir

    -- basedir <path>
    -- allows the game to run from outside the data tree
    Just baseDir <- CVar.get "basedir" "." Constants.cvarNoSet
    fsGlobals.fsBaseDir .= baseDir

    -- cddir <path>
    -- Logically concatenates the cddir after the basedir for
    -- allows the game to run from outside the data tree
    setCDDir

    -- start up with baseq2 by default
    addGameDirectory $ (baseDir^.cvString) `B.append` "/" `B.append` Constants.baseDirName

    makeBaseSearchPaths

    Just gameDirVar <- CVar.get "game" "" (Constants.cvarLatch .|. Constants.cvarServerInfo)
    fsGlobals.fsGameDirVar .= gameDirVar

    when (B.length (gameDirVar^.cvString) > 0) $ setGameDir (gameDirVar^.cvString)

createPath :: B.ByteString -> Quake ()
createPath path = do
    -- TODO: int index = path.lastIndexOf('/')
    --       // -1 if not found and 0 means write to root
    --       if (index > 0) ... then we create a directory

    done <- io (catchAny (createDirectoryIfMissing True (BC.unpack path) >> return (Right ())) $ \_ ->
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
addGameDirectory dir = do
    fsGlobals.fsGameDir .= dir

    -- add the directory to the search path
    -- ensure fs_userdir is first in searchpath
    let newSearchPath = newSearchPathT { _spFilename = dir }
    searchPaths <- use $ fsGlobals.fsSearchPaths
    case searchPaths of
      [] -> fsGlobals.fsSearchPaths .= [newSearchPath]
      (x:xs) -> fsGlobals.fsSearchPaths .= x : newSearchPath : xs

    -- add any pack files in the format pak0.pak pak1.pak ...
    void $ sequence $ fmap (addPackFiles dir) [0..9]

  where addPackFiles :: B.ByteString -> Int -> Quake ()
        addPackFiles directory i = do
          let pakFile = directory `B.append` "/pak" `B.append` BC.pack (show i) `B.append` ".pak" -- TODO: use binary package for Int to ByteString conversion?
              pakFileS = BC.unpack pakFile

          fileExists <- io $ doesFileExist pakFileS

          when fileExists $ do
            permissions <- io $ getPermissions pakFileS

            when (readable permissions) $ do
              pak <- loadPackFile pakFile

              when (isJust pak) $ do
                fsGlobals.fsSearchPaths %= (newSearchPathT { _spFilename = "", _spPack = pak } :)

loadPackFile :: B.ByteString -> Quake (Maybe PackT)
loadPackFile = undefined

-- set baseq2 directory
setCDDir :: Quake ()
setCDDir = do
    Just cdDir <- CVar.get "cddir" "" Constants.cvarArchive
    fsGlobals.fsCDDir .= cdDir
    when (B.length (cdDir^.cvString) > 0) $ addGameDirectory (cdDir^.cvString)

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
