module Sys.Sys where

import Control.Lens (use, (.=), (^.))
import qualified Data.ByteString as B

import Quake
import QuakeState
import qualified Sys.Timer as Timer

sysError :: B.ByteString -> Quake ()
sysError _ = io (putStrLn "Sys.sysError") >> undefined -- TODO

sendKeyEvents :: Quake ()
sendKeyEvents = do
    Just renderer <- use $ globals.re
    renderer^.rRefExport.reGetKeyboardHandler.kbdUpdate

    -- grab frame time
    time <- Timer.milliseconds
    globals.sysFrameTime .= time
