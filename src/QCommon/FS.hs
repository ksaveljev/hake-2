module QCommon.FS
    ( canRead
    , developerSearchPath
    , execAutoexec
    , fileLength
    , fOpenFile
    , fOpenFileWithLength
    , gameDir
    , initializeFileSystem
    , loadFile
    , setCDDir
    , setGameDir
    , markBaseSearchPaths
    ) where

import           Control.Lens               (use, (.=), (^.), (%=), (&), (.~))
import           Control.Monad              (when, unless)
import           Control.Monad.State.Strict (runStateT)
import           Data.Bits                  ((.|.))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import           Data.Char                  (toLower)
import qualified Data.HashMap.Lazy          as HM
import           Data.Sequence              ((|>), (><))
import qualified Data.Sequence              as Seq
import           Pipes.Binary               (deMessage)
import qualified Pipes.Binary               as PB
import qualified Pipes.ByteString           as PBS
import           System.Directory
import           System.IO                  (Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek), hSeek)

import qualified Constants
import qualified Game.Cmd                   as Cmd
import           Game.CVarT
import qualified QCommon.Com                as Com
import qualified QCommon.CVarShared         as CVar
import           QCommon.DPackHeaderT
import           QCommon.FileLinkT
import           QCommon.FSShared
import           QCommon.PackFileT
import           QCommon.PackT
import           QCommon.SearchPathT
import           QCommon.Shared
import           QuakeState
import           Types
import           Util.Binary                (encode)
import qualified Util.Lib                   as Lib

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands = [("path", Just pathF), ("link", Just linkF), ("dir", Just dirF)] 

initializeFileSystem :: Quake ()
initializeFileSystem = do
    Cmd.addInitialCommands initialCommands
    homeDir <- io getHomeDirectory
    let initialFsUserDir = BC.pack homeDir `B.append` "/.hake2"
    fsGlobals.fsUserDir .= initialFsUserDir
    createPath (initialFsUserDir `B.append` "/")
    addGameDirectory initialFsUserDir
    -- basedir <path>
    -- allows the game to run from outside the data tree
    baseDir <- CVar.get "basedir" "." Constants.cvarNoSet
    -- cddir <path>
    -- Logically concatenates the cddir after the basedir for
    -- allows the game to run from outside the data tree
    setCDDir
    maybe (Com.fatalError "basedir cvar not set") startWithBaseq2 baseDir

startWithBaseq2 :: CVarT -> Quake ()
startWithBaseq2 baseDir = do
    addGameDirectory (B.concat [baseDir^.cvString, "/", Constants.baseDirName])
    markBaseSearchPaths
    gameDirVar <- CVar.get "game" B.empty (Constants.cvarLatch .|. Constants.cvarServerInfo)
    maybe (Com.fatalError "game cvar not set") updateGameDir gameDirVar
  where
    updateGameDir dir =
        when (B.length (dir^.cvString) > 0) $
            setGameDir (dir^.cvString)

markBaseSearchPaths :: Quake ()
markBaseSearchPaths = do
    searchPaths <- use (fsGlobals.fsSearchPaths)
    fsGlobals.fsBaseSearchPaths .= searchPaths

setCDDir :: Quake ()
setCDDir = do
    cdDir <- CVar.get "cddir" B.empty Constants.cvarArchive
    maybe (Com.fatalError "cddir cvar not set") addDir cdDir
  where
    addDir cdDir = unless (B.null (cdDir^.cvString)) (addGameDirectory (cdDir^.cvString))

pathF :: XCommandT
pathF = XCommandT "FS.pathF" $ error "FS.pathF" -- TODO

linkF :: XCommandT
linkF = XCommandT "FS.linkF" $ error "FS.linkF" -- TODO

dirF :: XCommandT
dirF = XCommandT "FS.dirF" $ error "FS.dirF" -- TODO

createPath :: B.ByteString -> Quake ()
createPath path =
    maybe (return ()) proceedCreation ('/' `BC.elemIndexEnd` path)
  where
    proceedCreation idx = do
        let filePath = BC.unpack (B.take idx path)
        exists <- io (createDir filePath)
        unless exists $
            Com.printf (B.concat ["can't create path \"", path, "\n"])
    createDir filePath = do
        createDirectoryIfMissing True filePath
        doesDirectoryExist filePath

addGameDirectory :: B.ByteString -> Quake ()
addGameDirectory dir = do
    fsGlobals.fsGameDir .= dir
    addSearchPath dir
    mapM_ (addPackFiles dir) [0..9]

addSearchPath :: B.ByteString -> Quake ()
addSearchPath dir = do
    searchPaths <- use (fsGlobals.fsSearchPaths)
    fsGlobals.fsSearchPaths .= updateSearchPaths searchPaths
  where
    newSearchPath = SearchPathT dir Nothing
    updateSearchPaths searchPaths =
        case searchPaths of
            [] -> [newSearchPath]
            (x:xs) -> x : newSearchPath : xs

addPackFiles :: B.ByteString -> Int -> Quake ()
addPackFiles dir idx = do
    havePermissions <- io (canRead pakFileStr)
    when havePermissions proceedLoading
  where
    pakFile = B.concat [dir, "/pak", encode idx, ".pak"]
    pakFileStr = BC.unpack pakFile
    proceedLoading = do
        pak <- loadPackFile pakFile
        maybe (return ()) updateSearchPaths pak
    updateSearchPaths :: PackT -> Quake ()
    updateSearchPaths pak = fsGlobals.fsSearchPaths %= (newSearchPath pak :)
    newSearchPath pak = SearchPathT B.empty (Just pak)

loadPackFile :: B.ByteString -> Quake (Maybe PackT)
loadPackFile packFile = do
    fileHandle <- Lib.fOpenBinary packFile ReadMode
    maybe (return Nothing) (loadPackT packFile) fileHandle

loadPackT :: B.ByteString -> Handle -> Quake (Maybe PackT)
loadPackT packFile fileHandle = do
    header <- loadDPackHeaderT fileHandle
    loadPackFileTs packFile fileHandle header

loadPackFileTs :: B.ByteString -> Handle -> Maybe DPackHeaderT -> Quake (Maybe PackT)
loadPackFileTs _ _ Nothing = return Nothing
loadPackFileTs packFile fileHandle (Just header) = do
    (res, _) <- parsePackFiles
    either parsePackFilesError constructPackT res
  where
    numPackFiles = (header^.dphDirLen) `div` packFileSize
    parsePackFiles = do
        io (hSeek fileHandle AbsoluteSeek (fromIntegral (header^.dphDirOfs)))
        runStateT packFiles (PBS.fromHandle fileHandle)
    packFiles = PB.decodeGet (getPackFiles numPackFiles HM.empty)
    parsePackFilesError err = do
        Com.fatalError (BC.pack (deMessage err))
        return Nothing
    constructPackT files = do
        Com.printf (B.concat ["Added packfile ", packFile, " (", encode numPackFiles, " files)\n"])
        return (Just (PackT packFile (Just fileHandle) B.empty numPackFiles files))

loadDPackHeaderT :: Handle -> Quake (Maybe DPackHeaderT)
loadDPackHeaderT fileHandle = do
    (res, _) <- parseDPackHeaderT
    either dPackHeaderTParseError (return . Just) res
  where
    parseDPackHeaderT = do
        io (hSeek fileHandle AbsoluteSeek 0)
        runStateT dPackHeaderT (PBS.fromHandle fileHandle)
    dPackHeaderT = PB.decodeGet getDPackHeaderT
    dPackHeaderTParseError err = do
        Com.fatalError (BC.pack (deMessage err))
        return Nothing

getPackFiles :: Int -> HM.HashMap B.ByteString PackFileT -> PB.Get (HM.HashMap B.ByteString PackFileT)
getPackFiles numPackFiles files
    | numPackFiles == 0 = return files
    | otherwise = do
        file@(PackFileT name _ _) <- getPackFile
        getPackFiles (numPackFiles - 1) (HM.insert (BC.map toLower name) file files)

developerSearchPath :: Int -> Quake Int
developerSearchPath _ = do
    searchPaths <- use (fsGlobals.fsSearchPaths)
    return (findPath searchPaths)

findPath :: [SearchPathT] -> Int
findPath [] = 0
findPath (x:xs)
    | "xatrix" `BC.isInfixOf` (x^.spFilename) = 1
    | "rogue" `BC.isInfixOf` (x^.spFilename) = 2
    | otherwise = findPath xs