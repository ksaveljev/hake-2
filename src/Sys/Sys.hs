module Sys.Sys
    ( consoleOutput
    , quit
    , sendKeyEvents
    ) where

import           Control.Lens      (use, (^.), (.=))
import           Control.Monad     (when)
import           Data.ByteString   as B
import           System.Exit       (exitSuccess)

import           Client.RefExportT
import           Game.CVarT
import qualified QCommon.CVar      as CVar
import           QuakeState
import           Render.Renderer
import           Sys.KBD
import qualified Sys.Timer         as Timer
import           Types

import {-# SOURCE #-} qualified Client.CL as CL

consoleOutput :: B.ByteString -> Quake ()
consoleOutput msg = do
    noStdout <- CVar.findVar "nostdout"
    maybe printMsg (\v -> when ((v^.cvValue) /= 0) printMsg) noStdout
  where
    printMsg = io (B.putStr msg)

sendKeyEvents :: Quake ()
sendKeyEvents = do
    renderer <- use (globals.gRenderer)
    renderer^.rRefExport.reGetKeyboardHandler.kbdUpdate
    time <- Timer.milliseconds
    globals.gSysFrameTime .= time

quit :: Quake ()
quit = do
    CL.shutdown
    io exitSuccess
