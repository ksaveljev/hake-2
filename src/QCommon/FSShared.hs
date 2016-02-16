module QCommon.FSShared where

import           Types

import qualified Data.ByteString as B

loadFile :: B.ByteString -> Quake (Maybe B.ByteString)
loadFile = error "FS.loadFile" -- TODO