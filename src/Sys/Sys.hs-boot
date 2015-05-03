module Sys.Sys where

import qualified Data.ByteString as B

import Quake

sysError :: B.ByteString -> Quake ()

sendKeyEvents :: Quake ()

consoleOutput :: B.ByteString -> Quake ()
