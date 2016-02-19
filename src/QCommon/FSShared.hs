module QCommon.FSShared
  ( loadFile
  , canRead
  ) where

import qualified Constants
import qualified QCommon.Com as Com
import           QuakeState
import           Types

import           Control.Lens (use, (.=), (^.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (toLower)
import           Data.Foldable (find)
import qualified Data.HashMap.Lazy as HM
import           System.IO (Handle, hClose)
import           System.Directory
import           System.PosixCompat.Files (getFileStatus, fileSize)

loadFile :: B.ByteString -> Quake (Maybe B.ByteString)
loadFile path =
  -- IMPROVE: do we need this?
  {-// todo: hack for bad strings (fuck \0)
    int index = path.indexOf('\0');
    if (index != -1)
        path = path.substring(0, index); -}
  do len <- fileLength path
     openAndRead path len

openAndRead :: B.ByteString -> Int -> Quake (Maybe B.ByteString)
openAndRead path len
  | len < 1 = return Nothing
  | otherwise =
      do fileHandle <- fOpenFile path
         maybe openFailed readContents fileHandle
  where openFailed =
          do Com.comError Constants.errFatal "LoadFile failed"
             return Nothing
        readContents fileHandle =
          -- IMPROVE: catch exceptions
          request (do contents <- io (B.hGet fileHandle len)
                      io (hClose fileHandle)
                      return (Just contents))

fileLength :: B.ByteString -> Quake Int
fileLength fileName =
  do fsGlobals.fsFileFromPak .= 0
     links <- use (fsGlobals.fsLinks)
     maybe (fileLinkNotFound fileName) (fileLinkFound fileName) (findLink links)
  where findLink = find (fileLinkMatch fileName)

fileLinkNotFound :: B.ByteString -> Quake Int
fileLinkNotFound fileName =
  do searchPaths <- use (fsGlobals.fsSearchPaths)
     foundFileLen <- searchPathMatch fileName searchPaths
     maybe fileNotFound return foundFileLen
  where fileNotFound =
          do Com.dprintf (B.concat ["FindFile: can't find ", fileName, "\n"])
             return (-1)

fileLinkFound :: B.ByteString -> FileLinkT -> Quake Int
fileLinkFound fileName fileLink =
  do havePermissions <- request (io (canRead netPathStr))
     getFileSize havePermissions
  where netPath = (fileLink^.flTo) `B.append` B.take (fileLink^.flFromLength) fileName
        netPathStr = BC.unpack netPath
        getFileSize True =
          do Com.dprintf (B.concat ["link file: ", netPath, "\n"])
             size <- request (io (fmap fileSize (getFileStatus netPathStr)))
             return (fromIntegral size)
        getFileSize False = return (-1)

fOpenFile :: B.ByteString -> Quake (Maybe Handle)
fOpenFile = error "FSShared.fOpenFile"

canRead :: String -> IO Bool
canRead file =
  do exists <- doesFileExist file
     if exists then isReadable else return False
  where isReadable = fmap readable (getPermissions file)

fileLinkMatch :: B.ByteString -> FileLinkT -> Bool
fileLinkMatch name fileLink =
  let from = fileLink^.flFrom
      len = fileLink^.flFromLength
  in B.take len name == B.take len from

searchPathMatch :: B.ByteString -> [SearchPathT] -> Quake (Maybe Int)
searchPathMatch _ [] = return Nothing
searchPathMatch name searchPaths@(searchPath:xs) =
  maybe (checkDirectoryTree name searchPaths)
        (processPackFile name xs)
        (searchPath^.spPack)

checkDirectoryTree :: B.ByteString -> [SearchPathT] -> Quake (Maybe Int)
checkDirectoryTree _ [] =
  do Com.fatalError "FS.checkDirectoryTree illegal state"
     return Nothing
checkDirectoryTree name (searchPath:xs) =
  do havePermissions <- request (io (canRead netPathStr))
     getFileSize havePermissions
  where netPath = B.concat [searchPath^.spFilename, "/", name]
        netPathStr = BC.unpack netPath
        getFileSize True =
          do Com.dprintf (B.concat ["FindFile: ", netPath, "\n"])
             size <- request (io (fmap fileSize (getFileStatus netPathStr)))
             return (Just (fromIntegral size))
        getFileSize False = searchPathMatch name xs

processPackFile :: B.ByteString -> [SearchPathT] -> PackT -> Quake (Maybe Int)
processPackFile name searchPaths pack =
  maybe proceedSearching (packFileFound fileName pack) (HM.lookup fileName files)
  where fileName = BC.map toLower name
        files = pack^.pFiles
        proceedSearching = searchPathMatch name searchPaths

packFileFound :: B.ByteString -> PackT -> PackFileT -> Quake (Maybe Int)
packFileFound fileName pack entry =
  do fsGlobals.fsFileFromPak .= 1
     Com.dprintf (B.concat ["PackFile: ", pack^.pFilename, " : ", fileName, "\n"])
     havePermissions <- request (io (canRead (BC.unpack (pack^.pFilename))))
     getFileSize havePermissions
  where getFileSize True = return (Just (entry^.pfFileLen))
        getFileSize False =
          do Com.fatalError ("Couldn't reopen " `B.append` (pack^.pFilename))
             return Nothing
