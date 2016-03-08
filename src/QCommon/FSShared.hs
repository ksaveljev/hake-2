module QCommon.FSShared
  ( canRead
  , gameDir
  , fileLength
  , fOpenFile
  , fOpenFileWithLength
  , loadFile
  ) where

import qualified Constants
import qualified QCommon.Com as Com
import           QCommon.FileLinkT
import           QCommon.PackFileT
import           QCommon.PackT
import           QCommon.SearchPathT
import           QuakeState
import           Types

import           Control.Lens (use, (.=), (^.), (%=), (&), (.~))
import           Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (toLower)
import           Data.Foldable (find)
import qualified Data.HashMap.Lazy as HM
import           System.IO (Handle, hClose, hIsOpen, openBinaryFile, hSeek, IOMode(ReadMode), SeekMode(AbsoluteSeek))
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
     maybe (return Nothing) (openAndRead path) len

openAndRead :: B.ByteString -> Int -> Quake (Maybe B.ByteString)
openAndRead path len =
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

fOpenFileWithLength :: B.ByteString -> Quake (Maybe (Handle, Int))
fOpenFileWithLength fileName =
  do len <- fileLength fileName
     maybe (return Nothing) (openWithLength fileName) len

openWithLength :: B.ByteString -> Int -> Quake (Maybe (Handle, Int))
openWithLength fileName len =
  do fileHandle <- fOpenFile fileName
     maybe (return Nothing) (\h -> return (Just (h, len))) fileHandle

fOpenFile :: B.ByteString -> Quake (Maybe Handle)
fOpenFile fileName =
  searchForFile fileName fromFileLink fromPackFile fromDirectoryTree fileNotFound
  where fromFileLink netPath True = fromDirectoryTree netPath
        fromFileLink _ False = return Nothing
        fromPackFile pack entry True =
          request (io (do h <- openBinaryFile (BC.unpack (pack^.pFilename)) ReadMode
                          hSeek h AbsoluteSeek (fromIntegral (entry^.pfFilePos))
                          return (Just h)))
        fromPackFile pack _ False =
          do Com.fatalError ("Couldn't reopen " `B.append` (pack^.pFilename))
             return Nothing
        fromDirectoryTree netPath =
          do h <- request (io (openBinaryFile (BC.unpack netPath) ReadMode))
             return (Just h)
        fileNotFound = return Nothing

fileLength :: B.ByteString -> Quake (Maybe Int)
fileLength fileName =
  searchForFile fileName fromFileLink fromPackFile fromDirectoryTree fileNotFound
  where fromFileLink netPath True =
          do Com.dprintf (B.concat ["link file: ", netPath, "\n"])
             fmap (Just . fromIntegral) (getFileSize netPath)
        fromFileLink _ False = return Nothing
        fromPackFile _ entry True = return (Just (entry^.pfFileLen))
        fromPackFile pack _ False =
          do Com.fatalError ("Couldn't reopen " `B.append` (pack^.pFilename))
             return Nothing
        fromDirectoryTree netPath =
          do Com.dprintf (B.concat ["FindFile: ", netPath, "\n"])
             fmap (Just . fromIntegral) (getFileSize netPath)
        fileNotFound =
          do Com.dprintf (B.concat ["FindFile: can't find ", fileName, "\n"])
             return Nothing
        getFileSize netPath = request (io (fmap fileSize (getFileStatus (BC.unpack netPath))))

searchForFile :: B.ByteString
              -> (B.ByteString -> Bool -> Quake (Maybe a))
              -> (PackT -> PackFileT -> Bool -> Quake (Maybe a))
              -> (B.ByteString -> Quake (Maybe a))
              -> Quake (Maybe a)
              -> Quake (Maybe a)
searchForFile fileName fromFileLink fromPackFile fromDirectoryTree fileNotFound =
  do fsGlobals.fsFileFromPak .= 0
     links <- use (fsGlobals.fsLinks)
     maybe (fileLinkNotFound fileName fromPackFile fromDirectoryTree fileNotFound)
           (fileLinkFound fileName fromFileLink)
           (findLink links)
  where findLink = find (fileLinkMatch fileName)

fileLinkNotFound :: B.ByteString
                 -> (PackT -> PackFileT -> Bool -> Quake (Maybe a))
                 -> (B.ByteString -> Quake (Maybe a))
                 -> Quake (Maybe a)
                 -> Quake (Maybe a)
fileLinkNotFound fileName fromPackFile fromDirectoryTree fileNotFound =
  do searchPaths <- use (fsGlobals.fsSearchPaths)
     searchResult <- searchPathMatch fileName searchPaths fromPackFile fromDirectoryTree
     maybe fileNotFound (return . Just) searchResult

fileLinkFound :: B.ByteString
              -> (B.ByteString -> Bool -> Quake (Maybe a))
              -> FileLinkT
              -> Quake (Maybe a)
fileLinkFound fileName fromFileLink fileLink =
  do havePermissions <- request (io (canRead (BC.unpack netPath)))
     fromFileLink netPath havePermissions
  where netPath = (fileLink^.flTo) `B.append` B.take (fileLink^.flFromLength) fileName

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

searchPathMatch :: B.ByteString
                -> [SearchPathT]
                 -> (PackT -> PackFileT -> Bool -> Quake (Maybe a))
                 -> (B.ByteString -> Quake (Maybe a))
                -> Quake (Maybe a)
searchPathMatch _ [] _ _ = return Nothing
searchPathMatch name searchPaths@(searchPath:xs) fromPackFile fromDirectoryTree =
  maybe (checkDirectoryTree name searchPaths fromPackFile fromDirectoryTree)
        (processPackFile name xs fromPackFile fromDirectoryTree)
        (searchPath^.spPack)

checkDirectoryTree :: B.ByteString
                   -> [SearchPathT]
                   -> (PackT -> PackFileT -> Bool -> Quake (Maybe a))
                   -> (B.ByteString -> Quake (Maybe a))
                   -> Quake (Maybe a)
checkDirectoryTree _ [] _ _ =
  do Com.fatalError "FS.checkDirectoryTree illegal state"
     return Nothing
checkDirectoryTree name (searchPath:xs) fromPackFile fromDirectoryTree =
  do havePermissions <- request (io (canRead (BC.unpack netPath)))
     getResult havePermissions
  where netPath = B.concat [searchPath^.spFilename, "/", name]
        getResult True = fromDirectoryTree netPath
        getResult False = searchPathMatch name xs fromPackFile fromDirectoryTree

processPackFile :: B.ByteString
                -> [SearchPathT]
                 -> (PackT -> PackFileT -> Bool -> Quake (Maybe a))
                 -> (B.ByteString -> Quake (Maybe a))
                -> PackT
                -> Quake (Maybe a)
processPackFile _ [] _ _ _ =
  do Com.fatalError "FS.processPackFile illegal state"
     return Nothing
processPackFile name searchPaths@(searchPath:_) fromPackFile fromDirectoryTree pack =
  do needsUpdate <- maybe (return True) needToOpen (pack^.pHandle)
     when needsUpdate openPackHandle
     maybe proceedSearching (packFileFound fileName pack fromPackFile) (HM.lookup fileName files)
  where fileName = BC.map toLower name
        files = pack^.pFiles
        proceedSearching = searchPathMatch name searchPaths fromPackFile fromDirectoryTree
        needToOpen h = fmap not (request (io (hIsOpen h)))
        openPackHandle =
          do h <- request (io (openBinaryFile (BC.unpack (pack^.pFilename)) ReadMode))
             fsGlobals.fsSearchPaths %= map (\sp -> if sp == searchPath then updatedSp h else sp) -- IMPROVE? any way to do it better?
        updatedSp h = searchPath & spPack .~ Just (pack & pHandle .~ Just h)

packFileFound :: B.ByteString
              -> PackT
              -> (PackT -> PackFileT -> Bool -> Quake (Maybe a))
              -> PackFileT
              -> Quake (Maybe a)
packFileFound fileName pack fromPackFile entry =
  do fsGlobals.fsFileFromPak .= 1
     Com.dprintf (B.concat ["PackFile: ", pack^.pFilename, " : ", fileName, "\n"])
     havePermissions <- request (io (canRead (BC.unpack (pack^.pFilename))))
     fromPackFile pack entry havePermissions

gameDir :: Quake B.ByteString
gameDir =
  do userDir <- use (fsGlobals.fsUserDir)
     -- IMPROVE: decide if fsUserDir should be Maybe B.ByteString...
     return (if userDir /= "" then userDir else Constants.baseDirName)
