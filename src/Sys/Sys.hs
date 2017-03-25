module Sys.Sys
    ( quit
    , sendKeyEvents
    ) where

import           Control.Lens      (use, (^.), (.=))
import           System.Exit       (exitSuccess)

import {-# SOURCE #-} qualified Client.CL as CL
import           Client.RefExportT
import           QuakeState
import           Render.Renderer
import           Sys.KBD
import qualified Sys.Timer         as Timer
import           Types

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