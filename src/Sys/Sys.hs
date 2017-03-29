{-# LANGUAGE OverloadedStrings #-}
module Sys.Sys where

import Control.Lens (use, (.=), (^.))
import Control.Monad (unless)
import Data.Maybe (isJust, fromJust)
import System.Exit (exitSuccess)
import qualified Data.ByteString as B

import Sys.KBD
import Types
import QuakeState
import qualified Client.CL as CL
import {-# SOURCE #-} qualified QCommon.CVar as CVar
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

consoleOutput :: B.ByteString -> Quake ()
consoleOutput msg = do
    noStdout <- CVar.findVar "nostdout"

    unless (isJust noStdout && ((fromJust noStdout)^.cvValue) /= 0) $
      io (B.putStr msg)

quit :: Quake ()
quit = do
    CL.shutdown
    io $ exitSuccess
