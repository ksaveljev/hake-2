module QCommon.FS
  ( execAutoexec
  , initializeFileSystem
  , loadFile
  , setCDDir
  , setGameDir
  , markBaseSearchPaths
  ) where

import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.CVarShared as CVar
import           QCommon.FSShared
import           QuakeState
import           Types
import           Util.Binary (encode)

import           Control.Lens (use, (.=), (^.), (%=))
import           Control.Monad (when, unless)
import           Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           System.Directory

initialCommands :: [(B.ByteString, Maybe XCommandT)]
initialCommands = [("path", Just pathF), ("link", Just linkF), ("dir", Just dirF)] 
  
initializeFileSystem :: Quake ()
initializeFileSystem =
  do Cmd.addInitialCommands initialCommands
     homeDir <- request (io getHomeDirectory)
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
startWithBaseq2 baseDir =
  do addGameDirectory (B.concat [baseDir^.cvString, "/", Constants.baseDirName])
     markBaseSearchPaths
     gameDirVar <- CVar.get "game" "" (Constants.cvarLatch .|. Constants.cvarServerInfo)
     maybe (Com.fatalError "game cvar not set") updateGameDir gameDirVar
  where updateGameDir gameDir =
          when (B.length (gameDir^.cvString) > 0) $
            setGameDir (gameDir^.cvString)

setCDDir :: Quake ()
setCDDir =
  do cdDir <- CVar.get "cddir" "" Constants.cvarArchive
     maybe (Com.fatalError "cddir cvar not set") addDir cdDir
  where addDir cdDir = unless (B.null (cdDir^.cvString)) (addGameDirectory (cdDir^.cvString))

setGameDir :: B.ByteString -> Quake ()
setGameDir = error "FS.setGameDir" -- TODO

markBaseSearchPaths :: Quake ()
markBaseSearchPaths =
  do searchPaths <- use (fsGlobals.fsSearchPaths)
     fsGlobals.fsBaseSearchPaths .= searchPaths

pathF :: XCommandT
pathF = error "FS.pathF" -- TODO

linkF :: XCommandT
linkF = error "FS.linkF" -- TODO

dirF :: XCommandT
dirF = error "FS.dirF" -- TODO

createPath :: B.ByteString -> Quake ()
createPath path =
  maybe (return ()) proceedCreation ('/' `BC.elemIndexEnd` path)
  where proceedCreation idx =
          do let filePath = BC.unpack (B.take idx path)
             exists <- request (io (createDir filePath))
             unless exists $
               Com.printf (B.concat ["can't create path \"", path, "\n"])
        createDir filePath =
          do createDirectoryIfMissing True filePath
             doesDirectoryExist filePath

addGameDirectory :: B.ByteString -> Quake ()
addGameDirectory dir =
  do fsGlobals.fsGameDir .= dir
     addSearchPath dir
     mapM_ (addPackFiles dir) [0..9]

addSearchPath :: B.ByteString -> Quake ()
addSearchPath dir =
  do searchPaths <- use (fsGlobals.fsSearchPaths)
     fsGlobals.fsSearchPaths .= updateSearchPaths searchPaths
  where newSearchPath = SearchPathT dir Nothing
        updateSearchPaths searchPaths =
          case searchPaths of
            [] -> [newSearchPath]
            (x:xs) -> x : newSearchPath : xs

addPackFiles :: B.ByteString -> Int -> Quake ()
addPackFiles dir idx =
  do havePermissions <- request (io (canRead pakFileStr))
     when havePermissions proceedLoading
  where pakFile = B.concat [dir, "/pak", encode idx, ".pak"]
        pakFileStr = BC.unpack pakFile
        proceedLoading =
          do pak <- loadPackFile pakFile
             maybe (return ()) updateSearchPaths pak
        updateSearchPaths :: PackT -> Quake ()
        updateSearchPaths pak = fsGlobals.fsSearchPaths %= (newSearchPath pak :)
        newSearchPath pak = SearchPathT "" (Just pak)

loadPackFile :: B.ByteString -> Quake (Maybe PackT)
loadPackFile = error "FS.loadPackFile" -- TODO

execAutoexec :: Quake ()
execAutoexec = error "FS.execAutoexec" -- TODO
