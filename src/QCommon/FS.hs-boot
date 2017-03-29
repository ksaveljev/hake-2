module QCommon.FS where

import qualified Data.ByteString as B

import Types

execAutoexec :: Quake ()

setGameDir :: B.ByteString -> Quake ()

loadFile :: B.ByteString -> Quake (Maybe B.ByteString)

developerSearchPath :: Int -> Quake Int
