module Util.QuakeFile
    ( close
    , open
    , readString
    ) where

import qualified Data.ByteString as B
import           System.IO       (IOMode(ReadWriteMode))

import           Types
import qualified Util.Lib        as Lib

open :: B.ByteString -> Quake (Maybe QuakeFile)
open name = (QuakeFile <$>) <$> Lib.fOpen name ReadWriteMode

close :: QuakeFile -> Quake ()
close (QuakeFile h) = Lib.fClose h

readString :: QuakeFile -> IO (Maybe B.ByteString)
readString = error "QuakeFile.readString" -- TODO