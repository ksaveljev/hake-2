{-# LANGUAGE OverloadedStrings #-}
module QCommon.FS where

import Data.Char (isSpace, toLower)
import Data.Bits ((.|.))
import Data.Maybe (isJust)
import Data.Binary.Get (Get, getWord32le, runGet, getByteString)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Control.Lens ((^.), (.=), (%=), use)
import Control.Monad (when, void)
import Control.Exception
import System.Directory
import System.IO (openFile, IOMode(ReadMode))
import System.IO.MMap (mmapFileByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

import Quake
import Internal
import QuakeState
import QCommon.DPackHeaderT
import QCommon.PackFileT
import qualified Constants
import qualified QCommon.FSConstants as FSConstants
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

    markBaseSearchPaths

    Just gameDirVar <- CVar.get "game" "" (Constants.cvarLatch .|. Constants.cvarServerInfo)
    fsGlobals.fsGameDirVar .= gameDirVar

    when (B.length (gameDirVar^.cvString) > 0) $ setGameDir (gameDirVar^.cvString)

createPath :: B.ByteString -> Quake ()
createPath path = do
    -- IMPROVE: int index = path.lastIndexOf('/')
    --          // -1 if not found and 0 means write to root
    --          if (index > 0) ... then we create a directory

    done <- io (catchAny (createDirectoryIfMissing True (BC.unpack path) >> return (Right ())) $ \_ ->
      return $ Left ()) -- IMPROVE: maybe somehow include exception message?

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
          let pakFile = directory `B.append` "/pak" `B.append` BC.pack (show i) `B.append` ".pak" -- IMPROVE: use binary package for Int to ByteString conversion?
              pakFileS = BC.unpack pakFile

          fileExists <- io $ doesFileExist pakFileS

          when fileExists $ do
            permissions <- io $ getPermissions pakFileS

            when (readable permissions) $ do
              pak <- loadPackFile pakFile

              when (isJust pak) $
                fsGlobals.fsSearchPaths %= (newSearchPathT { _spFilename = "", _spPack = pak } :)

-- takes an explicit (not game tree related) path to a pak file
--
-- loads the header and directory, adding the files at the beginning of the
-- list so they override previous pack files
-- IMPROVE: there is a try catch block for IO operations in the original code ...
loadPackFile :: B.ByteString -> Quake (Maybe PackT)
loadPackFile packfile = do
    let filePath = BC.unpack packfile
    fileHandle <- io $ openFile filePath ReadMode
    fileContents <- io $ BL.fromStrict <$> mmapFileByteString filePath Nothing

    let header = runGet getDPackHeader fileContents

    when ((header^.dphIdent) /= FSConstants.idPakHeader) $
      Com.comError Constants.errFatal (packfile `B.append` " is not a packfile")

    let numPackFiles = header^.dphDirLen `div` packFileSize

    when (numPackFiles > FSConstants.maxFilesInPack) $
      Com.comError Constants.errFatal (packfile `B.append` " has "
                                                `B.append` BC.pack (show numPackFiles) -- IMPROVE: convert Int to ByteString using binary package?
                                                `B.append` " files")

    let directoryFiles = parseDirectory (BL.drop (fromIntegral $ header^.dphDirOfs) fileContents) numPackFiles M.empty
        pack = PackT packfile (Just fileHandle) "" numPackFiles directoryFiles

    Com.printf $ "Added packfile " `B.append` packfile
                                   `B.append` " ("
                                   `B.append` BC.pack (show numPackFiles) -- IMPROVE: convert Int to ByteString using binary package?
                                   `B.append` " files)\n"

    return $ Just pack

  where getInt :: Get Int
        getInt = fromIntegral <$> getWord32le

        getDPackHeader :: Get DPackHeaderT
        getDPackHeader = DPackHeaderT <$> getInt <*> getInt <*> getInt 

        getPackFile :: Get PackFileT
        getPackFile = do
          fileName <- strip <$> getByteString packFileNameSize
          filePos <- getInt
          fileLen <- getInt
          return $ PackFileT fileName filePos fileLen

        trimR :: B.ByteString -> B.ByteString
        trimR s = let rs = BC.reverse s in BC.dropWhile isSpace rs

        strip :: B.ByteString -> B.ByteString
        strip = trimR . trimR

        parseDirectory :: BL.ByteString -> Int -> M.Map B.ByteString PackFileT -> M.Map B.ByteString PackFileT
        parseDirectory _ 0 newFiles = newFiles
        parseDirectory fileContents numberOfFiles newFiles =
          let pf = runGet getPackFile fileContents
              lowercaseName = BC.map toLower (pf^.pfName)
          in parseDirectory (BL.drop (fromIntegral $ packFileNameSize + 8) fileContents) (numberOfFiles - 1) (M.insert lowercaseName pf newFiles)

-- set baseq2 directory
setCDDir :: Quake ()
setCDDir = do
    Just cdDir <- CVar.get "cddir" "" Constants.cvarArchive
    fsGlobals.fsCDDir .= cdDir
    when (B.length (cdDir^.cvString) > 0) $ addGameDirectory (cdDir^.cvString)

markBaseSearchPaths :: Quake ()
markBaseSearchPaths = do
    searchPaths <- use $ fsGlobals.fsSearchPaths
    fsGlobals.fsBaseSearchPaths .= searchPaths

setGameDir :: B.ByteString -> Quake ()
setGameDir = undefined -- TODO

pathF :: XCommandT
pathF = undefined -- TODO

linkF :: XCommandT
linkF = undefined -- TODO

dirF :: XCommandT
dirF = undefined -- TODO
