module QCommon.Shared where

import           Types

import qualified Data.ByteString as B

execAutoexec :: Quake ()
execAutoexec = error "FS.execAutoexec" -- TODO

setGameDir :: B.ByteString -> Quake ()
setGameDir = error "FS.setGameDir" -- TODO