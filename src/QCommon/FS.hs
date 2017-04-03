{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module QCommon.FS where

import Data.Char (toLower)
import Data.Bits ((.|.))
import Data.Maybe (isJust, fromJust)
import Data.Functor ((<$>))
import Data.Foldable (find)
import Data.Traversable (traverse)
import Control.Applicative ((<*>))
import Control.Lens ((^.), (.=), (%=), (&), (.~), use, set)
import Control.Monad (when, void, liftM, unless)
import Control.Exception
import System.Directory
import System.IO (openBinaryFile, hClose, hIsOpen, Handle, IOMode(ReadMode), hSeek, SeekMode(AbsoluteSeek))
import System.IO.MMap (mmapFileByteString)
import System.PosixCompat.Files (getFileStatus, fileSize)
import qualified Data.Sequence as Seq
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Lazy as HM

import Types
import QCommon.PackFileT
import QCommon.PackT
import QuakeState
import CVarVariables
import QCommon.XCommandT
import QCommon.DPackHeaderT
import QCommon.PackFileT
import Util.Binary
import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.CBuf as CBuf
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.FSConstants as FSConstants

initFileSystem :: Quake ()
initFileSystem = do
    Cmd.addCommand "path" (Just pathF)
    Cmd.addCommand "link" (Just linkF)
    Cmd.addCommand "dir" (Just dirF)

    homeDir <- io getHomeDirectory
    let initialFsUserDir = BC.pack homeDir `B.append` "/.hake2"
    fsGlobals.fsUserDir .= initialFsUserDir

    createPath $ initialFsUserDir `B.append` "/"
    addGameDirectory initialFsUserDir

    -- basedir <path>
    -- allows the game to run from outside the data tree
    Just baseDir <- CVar.get "basedir" "." Constants.cvarNoSet

    -- cddir <path>
    -- Logically concatenates the cddir after the basedir for
    -- allows the game to run from outside the data tree
    setCDDir

    -- start up with baseq2 by default
    addGameDirectory $ (baseDir^.cvString) `B.append` "/" `B.append` Constants.baseDirName

    markBaseSearchPaths

    Just gameDirVar <- CVar.get "game" "" (Constants.cvarLatch .|. Constants.cvarServerInfo)

    when (B.length (gameDirVar^.cvString) > 0) $ setGameDir (gameDirVar^.cvString)

createPath :: B.ByteString -> Quake ()
createPath path = do
    case '/' `BC.elemIndexEnd` path of
      Nothing ->
        return ()

      Just idx -> do
        let filePath = BC.unpack (B.take idx path)

        io $ createDirectoryIfMissing True filePath -- IMPROVE: catch exception?
        exists <- io $ doesDirectoryExist filePath

        unless exists $
          Com.printf ("can't create path \"" `B.append` path `B.append` "\n")

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
    fileHandle <- io $ openBinaryFile filePath ReadMode
    fileContents <- io $ BL.fromStrict <$> mmapFileByteString filePath Nothing

    let header = runGet getDPackHeader fileContents

    when ((header^.dphIdent) /= FSConstants.idPakHeader) $
      Com.comError Constants.errFatal (packfile `B.append` " is not a packfile")

    let numPackFiles = header^.dphDirLen `div` packFileSize

    when (numPackFiles > FSConstants.maxFilesInPack) $
      Com.comError Constants.errFatal (packfile `B.append` " has "
                                                `B.append` BC.pack (show numPackFiles) -- IMPROVE: convert Int to ByteString using binary package?
                                                `B.append` " files")

    let directoryFiles = parseDirectory (BL.drop (fromIntegral $ header^.dphDirOfs) fileContents) numPackFiles HM.empty
        pack = PackT packfile (Just fileHandle) "" numPackFiles directoryFiles

    Com.printf $ "Added packfile " `B.append` packfile
                                   `B.append` " ("
                                   `B.append` BC.pack (show numPackFiles) -- IMPROVE: convert Int to ByteString using binary package?
                                   `B.append` " files)\n"

    return $ Just pack

  where getDPackHeader :: Get DPackHeaderT
        getDPackHeader = DPackHeaderT <$> getInt <*> getInt <*> getInt 

        getPackFile :: Get PackFileT
        getPackFile = do
          fileName <- strip <$> getByteString packFileNameSize
          filePos <- getInt
          fileLen <- getInt
          return $ PackFileT fileName filePos fileLen

        strip :: B.ByteString -> B.ByteString
        strip = B.takeWhile (/= 0)

        parseDirectory :: BL.ByteString -> Int -> HM.HashMap B.ByteString PackFileT -> HM.HashMap B.ByteString PackFileT
        parseDirectory _ 0 newFiles = newFiles
        parseDirectory fileContents numberOfFiles newFiles =
          let pf = runGet getPackFile fileContents
              lowercaseName = BC.map toLower (pf^.pfName)
          in parseDirectory (BL.drop (fromIntegral $ packFileNameSize + 8) fileContents) (numberOfFiles - 1) (HM.insert lowercaseName pf newFiles)

-- set baseq2 directory
setCDDir :: Quake ()
setCDDir = do
    Just cdDir <- CVar.get "cddir" "" Constants.cvarArchive
    when (B.length (cdDir^.cvString) > 0) $ addGameDirectory (cdDir^.cvString)

markBaseSearchPaths :: Quake ()
markBaseSearchPaths = do
    searchPaths <- use $ fsGlobals.fsSearchPaths
    fsGlobals.fsBaseSearchPaths .= searchPaths

setGameDir :: B.ByteString -> Quake ()
setGameDir dir = do
    let invalidDir = or $ fmap (`B.isInfixOf` dir) ["..", "/", "\\", ":"]

    if invalidDir
      then Com.printf "Gamedir should be a single filename, not a path\n"
      else do
        -- free up any current game dir info
        searchPaths <- use $ fsGlobals.fsSearchPaths
        mapM_ freeSearchPath searchPaths
        fsGlobals.fsSearchPaths .= []

        -- flush all data, so it will be forced to reload
        dedicatedValue <- liftM (^.cvValue) dedicatedCVar
        when (dedicatedValue == 0) $
          CBuf.addText "vid_restart\nsnd_restart\n"

        baseDir <- liftM (^.cvString) fsBaseDirCVar
        fsGlobals.fsGameDir .= baseDir `B.append` "/" `B.append` dir

        if dir == Constants.baseDirName || B.length dir == 0
          then do
            void $ CVar.fullSet "gamedir" "" (Constants.cvarServerInfo .|. Constants.cvarNoSet)
            void $ CVar.fullSet "game" "" (Constants.cvarLatch .|. Constants.cvarServerInfo)
          else do
            void $ CVar.fullSet "gamedir" dir (Constants.cvarServerInfo .|. Constants.cvarNoSet)
            cddir <- liftM (^.cvString) fsCdDirCVar
            when (B.length cddir > 0) $
              addGameDirectory (cddir `B.append` "/" `B.append` dir)
            addGameDirectory (baseDir `B.append` "/" `B.append` dir)

  where freeSearchPath :: SearchPathT -> Quake ()
        freeSearchPath searchPath =
          when (isJust (searchPath^.spPack)) $ do
            let sp = fromJust (searchPath^.spPack)
            when (isJust (sp^.pHandle)) $ do
              let h = fromJust (sp^.pHandle)
              err <- io $ handle (\(e :: IOException) -> return (Just e)) (hClose h >> return Nothing)
              case err of
                Nothing -> return ()
                Just e -> Com.dprintf (BC.pack $ show e)

pathF :: XCommandT
pathF =
  XCommandT "FS.pathF" (do
    Com.printf "Current search path:\n"
    searchPaths <- use $ fsGlobals.fsSearchPaths

    void $ traverse printSearchPath searchPaths

    Com.printf "\nLinks:\n"
    links <- use $ fsGlobals.fsLinks

    void $ traverse printLink links
  )

  where printLink link =
          Com.printf $ (link^.flFrom) 
            `B.append` " : "
            `B.append` (link^.flTo)
            `B.append` "\n"

        printSearchPath sp =
          {- IMPROVE:
          - if (s == fs_base_searchpaths)
          -     Com.Printf("----------\n");
          -}
          if isJust (sp^.spPack)
            then do
              let Just s = sp^.spPack
              Com.printf $ (s^.pFilename)
                `B.append` " ("
                `B.append` BC.pack (show (s^.pNumFiles)) -- IMPROVE: convert Int to ByteString using binary package?
                `B.append` " files)\n"
            else Com.printf $ (sp^.spFilename) `B.append` "\n"

linkF :: XCommandT
linkF =
  XCommandT "FS.linkF" (do
    c <- Cmd.argc

    if c /= 3
      then Com.printf "USAGE: link <from> <to>\n"
      else do
        v1 <- Cmd.argv 1
        v2 <- Cmd.argv 2
        links <- use $ fsGlobals.fsLinks
        let existingLinkIdx = Seq.findIndexL (\link -> (link^.flFrom) == v1) links

        if | isJust existingLinkIdx -> do
               let idx = fromJust existingLinkIdx

               if B.length v2 < 1
                 then -- delete it
                   fsGlobals.fsLinks %= (\fsl -> Seq.take idx fsl Seq.>< Seq.drop (idx + 1) fsl)
                 else do
                   let link = Seq.index links idx
                   fsGlobals.fsLinks %= Seq.update idx (link { _flTo = v2 })

           | B.length v2 > 0 ->
               fsGlobals.fsLinks %= (Seq.|> FileLinkT v1 (B.length v1) v2)

           | otherwise -> return ()
  )

dirF :: XCommandT
dirF =
  XCommandT "FS.dirF" (do
    --c <- Cmd.argc

    --wildcard <- if c /= 1 then Cmd.argv 1 else return "*.*"

    io (putStrLn "FS.dirF") >> undefined -- TODO
  )

execAutoexec :: Quake ()
execAutoexec = do
    dir <- use $ fsGlobals.fsUserDir

    name <- if B.length dir > 0
              then return (dir `B.append` "/autoexec.cfg")
              else do
                baseDir <- liftM (^.cvString) fsBaseDirCVar
                return (baseDir `B.append` "/" `B.append` Constants.baseDirName `B.append` "/autoexec.cfg")

    -- TODO: are we sure it is enough to check for file existance only?
    -- let cantHave = Constants.sffSubDir .|. Constants.sffHidden .|. Constants.sffSystem
    fileExists <- io $ doesFileExist (BC.unpack name)

    when fileExists $
      CBuf.addText "exec autoexec.cfg\n"

{-
- Gamedir
- 
- Called to find where to write a file (demos, savegames, etc)
- this is modified to <user.home>/.hake2 
-}
gameDir :: Quake B.ByteString
gameDir = do
    userDir <- use $ fsGlobals.fsUserDir
    -- TODO: decide if fsUserDir should be Maybe B.ByteString...
    return $ if userDir /= ""
               then userDir
               else Constants.baseDirName

{-
- FOpenFile
- 
- Finds the file in the search path. returns a Handle. Used for
- streaming data out of either a pak file or a seperate file.
-}
fOpenFile :: B.ByteString -> Quake (Maybe Handle)
fOpenFile filename = do
    fsGlobals.fsFileFromPak .= 0

    -- check for links first
    links <- use $ fsGlobals.fsLinks
    let foundLink = find (fileLinkMatch filename) links

    case foundLink of
      Just filelink -> do
        let netpath = (filelink^.flTo) `B.append` B.take (filelink^.flFromLength) filename
            netpathUnpacked = BC.unpack netpath

        fileExists <- io $ doesFileExist netpathUnpacked
        canRead <- if fileExists
                    then io $ liftM readable (getPermissions netpathUnpacked)
                    else return False
        if canRead
          then do
            -- Com.dprintf $ "link file: " `B.append` netpath `B.append` "\n"
            h <- io $ openBinaryFile netpathUnpacked ReadMode
            return $ Just h
          else return Nothing

      -- search through the path, one element at a time
      Nothing -> do
        searchPaths <- use $ fsGlobals.fsSearchPaths
        searchPathMatch filename searchPaths

  -- IMPROVE: pretty much the same as in fileLength. Reduce duplication
  where searchPathMatch :: B.ByteString -> [SearchPathT] -> Quake (Maybe Handle)
        searchPathMatch _ [] = return Nothing
        searchPathMatch name (searchPath:xs) =
          -- is the element a pak file?
          case searchPath^.spPack of
            Just pack -> do -- look through all the pak file elements
              let fname = BC.map toLower name
                  files = pack^.pFiles

              case HM.lookup fname files of
                Just entry -> do -- found it!
                  fsGlobals.fsFileFromPak .= 1

                  --Com.dprintf $ "PackFile: " `B.append` (pack^.pFilename) `B.append` " : " `B.append` fname `B.append` "\n"
                  fileExists <- io $ doesFileExist (BC.unpack $ pack^.pFilename)
                  canRead <- if fileExists
                              then io $ liftM readable (getPermissions (BC.unpack $ pack^.pFilename))
                              else return False

                  unless canRead $
                    Com.comError Constants.errFatal ("Couldn't reopen " `B.append` (pack^.pFilename))

                  needsUpdate <- case pack^.pHandle of
                                   Nothing -> return True
                                   Just h -> do
                                     isOpen <- io $ hIsOpen h
                                     return $ not isOpen

                  when needsUpdate $ do
                    h <- io $ openBinaryFile (BC.unpack $ pack^.pFilename) ReadMode
                    let updatedSP = set spPack (Just $ pack { _pHandle = Just h }) searchPath
                    fsGlobals.fsSearchPaths %= map (\sp -> if sp == searchPath then updatedSP else sp) -- IMPROVE: not nice, but how else would we do it?

                  -- open a new file on the pakfile
                  h <- io $ openBinaryFile (BC.unpack $ pack^.pFilename) ReadMode
                  io $ hSeek h AbsoluteSeek (fromIntegral $ entry^.pfFilePos)
                  return $ Just h

                Nothing -> searchPathMatch name xs

            Nothing -> do
              -- check a file in the directory tree
              let netpath = (searchPath^.spFilename) `B.append` "/" `B.append` name
                  netpathUnpacked = BC.unpack netpath

              fileExists <- io $ doesFileExist netpathUnpacked
              canRead <- if fileExists
                          then io $ liftM readable (getPermissions netpathUnpacked)
                          else return False

              if canRead
                then do
                  -- Com.dprintf $ "FindFile: " `B.append` netpath `B.append` "\n"
                  h <- io $ openBinaryFile netpathUnpacked ReadMode
                  return $ Just h
                else searchPathMatch name xs

developerSearchPath :: Int -> Quake Int
developerSearchPath _ = do
    searchPaths <- use $ fsGlobals.fsSearchPaths
    return $ findPath searchPaths

  where findPath :: [SearchPathT] -> Int
        findPath [] = 0
        findPath (x:xs) = if | "xatrix" `BC.isInfixOf` (x^.spFilename) -> 1
                             | "rogue" `BC.isInfixOf` (x^.spFilename) -> 2
                             | otherwise -> findPath xs

loadFile :: B.ByteString -> Quake (Maybe B.ByteString)
loadFile path = do
  -- IMPROVE: do we need this?
  {-// todo: hack for bad strings (fuck \0)
    int index = path.indexOf('\0');
    if (index != -1)
        path = path.substring(0, index); -}
    len <- fileLength path
    maybe (return Nothing) (openAndRead path) len

openAndRead :: B.ByteString -> Int -> Quake (Maybe B.ByteString)
openAndRead path len = do
    fileHandle <- fOpenFile path
    maybe openFailed readContents fileHandle
  where
    openFailed = do
        Com.comError Constants.errFatal "LoadFile failed"
        return Nothing
    readContents fileHandle = do
        -- IMPROVE: catch exceptions
        contents <- io (B.hGet fileHandle len)
        io (hClose fileHandle)
        return (Just contents)

fOpenFileWithLength :: B.ByteString -> Quake (Maybe (Handle, Int))
fOpenFileWithLength fileName = do
    len <- fileLength fileName
    maybe (return Nothing) (openWithLength fileName) len

openWithLength :: B.ByteString -> Int -> Quake (Maybe (Handle, Int))
openWithLength fileName len = do
    fileHandle <- fOpenFile fileName
    maybe (return Nothing) (\h -> return (Just (h, len))) fileHandle

fileLength :: B.ByteString -> Quake (Maybe Int)
fileLength fileName =
    searchForFile fileName fromFileLink fromPackFile fromDirectoryTree fileNotFound
  where
    fromFileLink netPath True = do
        Com.dprintf (B.concat ["link file: ", netPath, "\n"])
        fmap (Just . fromIntegral) (getFileSize netPath)
    fromFileLink _ False = return Nothing
    fromPackFile _ entry True = return (Just (entry^.pfFileLen))
    fromPackFile pack _ False = do
        Com.fatalError ("Couldn't reopen " `B.append` (pack^.pFilename))
        return Nothing
    fromDirectoryTree netPath = do
        Com.dprintf (B.concat ["FindFile: ", netPath, "\n"])
        fmap (Just . fromIntegral) (getFileSize netPath)
    fileNotFound = do
        Com.dprintf (B.concat ["FindFile: can't find ", fileName, "\n"])
        return Nothing
    getFileSize netPath = io (fmap fileSize (getFileStatus (BC.unpack netPath)))

searchForFile :: B.ByteString
              -> (B.ByteString -> Bool -> Quake (Maybe a))
              -> (PackT -> PackFileT -> Bool -> Quake (Maybe a))
              -> (B.ByteString -> Quake (Maybe a))
              -> Quake (Maybe a)
              -> Quake (Maybe a)
searchForFile fileName fromFileLink fromPackFile fromDirectoryTree fileNotFound = do
    fsGlobals.fsFileFromPak .= 0
    links <- use (fsGlobals.fsLinks)
    maybe (fileLinkNotFound fileName fromPackFile fromDirectoryTree fileNotFound)
          (fileLinkFound fileName fromFileLink)
          (findLink links)
  where
    findLink = find (fileLinkMatch fileName)

fileLinkNotFound :: B.ByteString
                 -> (PackT -> PackFileT -> Bool -> Quake (Maybe a))
                 -> (B.ByteString -> Quake (Maybe a))
                 -> Quake (Maybe a)
                 -> Quake (Maybe a)
fileLinkNotFound fileName fromPackFile fromDirectoryTree fileNotFound = do
    searchPaths <- use (fsGlobals.fsSearchPaths)
    searchResult <- searchPathMatch fileName searchPaths fromPackFile fromDirectoryTree
    maybe fileNotFound (return . Just) searchResult

fileLinkFound :: B.ByteString
              -> (B.ByteString -> Bool -> Quake (Maybe a))
              -> FileLinkT
              -> Quake (Maybe a)
fileLinkFound fileName fromFileLink fileLink = do
    havePermissions <- io (canRead (BC.unpack netPath))
    fromFileLink netPath havePermissions
  where
    netPath = (fileLink^.flTo) `B.append` B.take (fileLink^.flFromLength) fileName

canRead :: String -> IO Bool
canRead file = do
    exists <- doesFileExist file
    if exists then isReadable else return False
  where
    isReadable = fmap readable (getPermissions file)

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
checkDirectoryTree _ [] _ _ = do
    Com.fatalError "FS.checkDirectoryTree illegal state"
    return Nothing
checkDirectoryTree name (searchPath:xs) fromPackFile fromDirectoryTree = do
    havePermissions <- io (canRead (BC.unpack netPath))
    getResult havePermissions
  where
    netPath = B.concat [searchPath^.spFilename, "/", name]
    getResult True = fromDirectoryTree netPath
    getResult False = searchPathMatch name xs fromPackFile fromDirectoryTree

processPackFile :: B.ByteString
                -> [SearchPathT]
                 -> (PackT -> PackFileT -> Bool -> Quake (Maybe a))
                 -> (B.ByteString -> Quake (Maybe a))
                -> PackT
                -> Quake (Maybe a)
processPackFile _ [] _ _ _ = do
    Com.fatalError "FS.processPackFile illegal state"
    return Nothing
processPackFile name searchPaths@(searchPath:_) fromPackFile fromDirectoryTree pack = do
    needsUpdate <- maybe (return True) needToOpen (pack^.pHandle)
    when needsUpdate openPackHandle
    maybe proceedSearching (packFileFound fileName pack fromPackFile) (HM.lookup fileName files)
  where
    fileName = BC.map toLower name
    files = pack^.pFiles
    proceedSearching = searchPathMatch name searchPaths fromPackFile fromDirectoryTree
    needToOpen h = fmap not (io (hIsOpen h))
    openPackHandle = do
        h <- io (openBinaryFile (BC.unpack (pack^.pFilename)) ReadMode)
        fsGlobals.fsSearchPaths %= map (\sp -> if sp == searchPath then updatedSp h else sp) -- IMPROVE? any way to do it better?
    updatedSp h = searchPath & spPack .~ Just (pack & pHandle .~ Just h)

packFileFound :: B.ByteString
              -> PackT
              -> (PackT -> PackFileT -> Bool -> Quake (Maybe a))
              -> PackFileT
              -> Quake (Maybe a)
packFileFound fileName pack fromPackFile entry = do
    fsGlobals.fsFileFromPak .= 1
    Com.dprintf (B.concat ["PackFile: ", pack^.pFilename, " : ", fileName, "\n"])
    havePermissions <- io (canRead (BC.unpack (pack^.pFilename)))
    fromPackFile pack entry havePermissions