module QCommon.Shared where

import           Control.Lens          (use, (^.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           System.Directory      (doesFileExist)

import qualified Constants
import           Game.CVarT
import qualified QCommon.CBuf          as CBuf
import           QCommon.CVarVariables
import           QuakeState
import           Types

execAutoexec :: Quake ()
execAutoexec = do
    name <- autoexecFileName =<< use (fsGlobals.fsUserDir)
    autoexec =<< io (doesFileExist (BC.unpack name))
  where
    autoexecFileName dir
        | not (B.null dir) = return (dir `B.append` "/autoexec.cfg")
        | otherwise = do
            baseDir <- fsBaseDirCVar
            return (B.concat [baseDir^.cvString, "/", Constants.baseDirName, "/autoexec.cfg"])
    autoexec False = return ()
    autoexec True = CBuf.addText "exec autoexec.cfg\n"

setGameDir :: B.ByteString -> Quake ()
setGameDir = error "FS.setGameDir" -- TODO