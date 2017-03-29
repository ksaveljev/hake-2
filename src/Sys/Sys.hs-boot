module Sys.Sys where

import qualified Data.ByteString as B

import Types

sysError :: B.ByteString -> Quake ()

sendKeyEvents :: Quake ()

consoleOutput :: B.ByteString -> Quake ()

quit :: Quake ()
