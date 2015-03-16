{-# LANGUAGE ScopedTypeVariables #-}
module Util.QuakeFile ( QuakeFile
                      , open
                      , close
                      , writeString
                      , readString
                      ) where

import Data.Functor ((<$>))
import Data.Binary.Get
import Data.Binary.Put
import System.IO (openFile, IOMode(ReadWriteMode), Handle, hClose)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC

data QuakeFile = QuakeFile Handle

open :: B.ByteString -> IO QuakeFile
open name = QuakeFile <$> openFile (BC.unpack name) ReadWriteMode

close :: QuakeFile -> IO ()
close (QuakeFile h) = hClose h

writeString :: QuakeFile -> B.ByteString -> IO ()
writeString (QuakeFile h) str =
    BL.hPut h $ runPut $ do
      putWord32le (fromIntegral $ B.length str)
      putByteString str

readString :: QuakeFile -> IO B.ByteString
readString (QuakeFile h) = do
    stringSize <- BL.hGet h 4
    let len :: Int = runGet (fromIntegral <$> getWord32le) stringSize
    B.hGet h len
