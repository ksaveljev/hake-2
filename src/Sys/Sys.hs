module Sys.Sys where

import Quake
import qualified Data.ByteString as B

sysError :: B.ByteString -> Quake ()
sysError _ = io (putStrLn "Sys.sysError") >> undefined -- TODO

sendKeyEvents :: Quake ()
sendKeyEvents = io (putStrLn "Sys.sendKeyEvents") >> undefined -- TODO
