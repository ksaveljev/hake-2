module QCommon.FS
  (initializeFileSystem
  ,loadFile
  ,setCDDir
  ,markBaseSearchPaths)
  where

import qualified Constants
import qualified Game.Cmd as Cmd
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.FSShared
import           QuakeState
import           Types

import           Control.Lens ((.=), (^.))
import           Control.Monad (when)
import           Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           System.Directory (getHomeDirectory)

initialCommands :: [(B.ByteString,XCommandT)]
initialCommands =
  [("path",pathF),("link",linkF),("dir",dirF)] 
  
initializeCommands :: Quake ()
initializeCommands = mapM_ (\(name,cmd) -> Cmd.addCommand name (Just cmd)) initialCommands

initializeFileSystem :: Quake ()
initializeFileSystem =
  do initializeCommands
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
     maybe (Com.comError Constants.errFatal "basedir cvar not set") startWithBaseq2 baseDir

startWithBaseq2 :: CVarT -> Quake ()
startWithBaseq2 baseDir =
  do addGameDirectory ((baseDir^.cvString) `B.append` "/" `B.append` Constants.baseDirName)
     markBaseSearchPaths
     gameDirVar <- CVar.get "game" "" (Constants.cvarLatch .|. Constants.cvarServerInfo)
     maybe (Com.comError Constants.errFatal "game cvar not set") updateGameDir gameDirVar
  where updateGameDir gameDir =
          when (B.length (gameDir^.cvString) > 0) $
            setGameDir (gameDir^.cvString)

setCDDir :: Quake ()
setCDDir = error "FS.setCDDir" -- TODO

setGameDir :: B.ByteString -> Quake ()
setGameDir = error "FS.setGameDir" -- TODO

markBaseSearchPaths :: Quake ()
markBaseSearchPaths = error "FS.markBaseSearchPaths" -- TODO

pathF :: XCommandT
pathF = error "FS.pathF" -- TODO

linkF :: XCommandT
linkF = error "FS.linkF" -- TODO

dirF :: XCommandT
dirF = error "FS.dirF" -- TODO

createPath :: B.ByteString -> Quake ()
createPath = error "FS.createPath" -- TODO

addGameDirectory :: B.ByteString -> Quake ()
addGameDirectory = error "FS.addGameDirectory" -- TODO