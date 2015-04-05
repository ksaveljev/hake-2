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
import Control.Lens ((^.), (.=), (%=), use, set)
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
import qualified Data.Map as M

import Quake
import QuakeState
import CVarVariables
import QCommon.XCommandT
import QCommon.DPackHeaderT
import QCommon.PackFileT
import Util.Binary
import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.CBuf as CBuf
import qualified QCommon.Com as Com
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

    let directoryFiles = parseDirectory (BL.drop (fromIntegral $ header^.dphDirOfs) fileContents) numPackFiles M.empty
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
pathF = do
    Com.printf "Current search path:\n"
    searchPaths <- use $ fsGlobals.fsSearchPaths

    void $ traverse printSearchPath searchPaths

    Com.printf "\nLinks:\n"
    links <- use $ fsGlobals.fsLinks

    void $ traverse printLink links

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
linkF = do
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

dirF :: XCommandT
dirF = do
    --c <- Cmd.argc

    --wildcard <- if c /= 1 then Cmd.argv 1 else return "*.*"

    io (putStrLn "FS.dirF") >> undefined -- TODO

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
- LoadFile
- 
- Filename are reletive to the quake search path a null buffer will just
- return the file content as byte[]
-}
loadFile :: B.ByteString -> Quake (Maybe B.ByteString)
loadFile path = do
    -- IMPROVE: do we need this?
    {-
    // todo: hack for bad strings (fuck \0)
    int index = path.indexOf('\0');
    if (index != -1)
        path = path.substring(0, index);
    -}

    -- look for it in the filesystem or pack files
    len <- fileLength path

    if len < 1
      then return Nothing
      else do
        -- IMPROVE: catch exceptions:
        -- } catch (IOException e) {
        --    Com.Error(Defines.ERR_FATAL, e.toString());
        -- }
        file <- fOpenFile path

        case file of
          Nothing -> do
            Com.comError Constants.errFatal "LoadFile failed"
            return Nothing
          Just h -> do
            contents <- io $ B.hGet h len
            io $ hClose h
            return $ Just contents

{-
- Gamedir
- 
- Called to find where to write a file (demos, savegames, etc)
- this is modified to <user.home>/.hake2 
-}
gameDir :: Quake B.ByteString
gameDir = do
    userDir <- use $ fsGlobals.fsUserDir
    -- TODO: dedice if fsUserDir should be Maybe B.ByteString...
    return $ if userDir /= ""
               then userDir
               else Constants.baseDirName

fileLength :: B.ByteString -> Quake Int
fileLength filename = do
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
            Com.dprintf $ "link file: " `B.append` netpath `B.append` "\n"
            size <- io $ liftM fileSize (getFileStatus netpathUnpacked)
            return (fromIntegral size)
          else return (-1)

      -- search through the path, one element at a time
      Nothing -> do
        searchPaths <- use $ fsGlobals.fsSearchPaths
        foundFileLen <- searchPathMatch filename searchPaths

        case foundFileLen of
          Nothing -> do
            Com.dprintf $ "FindFile: can't find " `B.append` filename `B.append` "\n"
            return (-1)
          Just fileLen -> return fileLen

  where searchPathMatch :: B.ByteString -> [SearchPathT] -> Quake (Maybe Int)
        searchPathMatch _ [] = return Nothing
        searchPathMatch name (searchPath:xs) =
          -- is the element a pak file?
          case searchPath^.spPack of
            Just pack -> do -- look through all the pak file elements
              let fname = BC.map toLower name
                  files = pack^.pFiles

              case M.lookup fname files of
                Just entry -> do -- found it!
                  fsGlobals.fsFileFromPak .= 1

                  Com.dprintf $ "PackFile: " `B.append` (pack^.pFilename) `B.append` " : " `B.append` fname `B.append` "\n"
                  -- open a new file on the pakfile
                  fileExists <- io $ doesFileExist (BC.unpack $ pack^.pFilename)
                  canRead <- if fileExists
                              then io $ liftM readable (getPermissions (BC.unpack $ pack^.pFilename))
                              else return False

                  unless canRead $
                    Com.comError Constants.errFatal ("Couldn't reopen " `B.append` (pack^.pFilename))

                  return $ Just (entry^.pfFileLen)

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
                  Com.dprintf $ "FindFile: " `B.append` netpath `B.append` "\n"
                  size <- io $ liftM fileSize (getFileStatus netpathUnpacked)
                  return $ Just (fromIntegral size)
                else searchPathMatch name xs

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

              case M.lookup fname files of
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

                  if needsUpdate
                    then do
                      h <- io $ openBinaryFile (BC.unpack $ pack^.pFilename) ReadMode
                      let updatedSP = set spPack (Just $ pack { _pHandle = Just h }) searchPath
                      fsGlobals.fsSearchPaths %= map (\sp -> if sp == searchPath then updatedSP else sp) -- IMPROVE: not nice, but how else would we do it?
                      io $ hSeek h AbsoluteSeek (fromIntegral $ entry^.pfFilePos)
                      return $ Just h
                    else do
                      io $ hSeek (fromJust $ pack^.pHandle) AbsoluteSeek (fromIntegral $ entry^.pfFilePos)
                      return (pack^.pHandle)

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

fileLinkMatch :: B.ByteString -> FileLinkT -> Bool
fileLinkMatch name fileLink =
    let from = fileLink^.flFrom
        len = fileLink^.flFromLength
    in B.take len name == B.take len from
