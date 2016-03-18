module Sys.Sys
  ( sendKeyEvents
  ) where

import           Client.RefExportT
import           QuakeState
import           Render.Renderer
import           Sys.KBD
import qualified Sys.Timer as Timer
import           Types

import           Control.Lens (use, (^.), (.=))

sendKeyEvents :: Quake ()
sendKeyEvents =
  do renderer <- use (globals.gRenderer)
     maybe rendererError doSendKeyEvents renderer
  where rendererError = error "Sys.sendKeyEvents renderer is Nothing"
        doSendKeyEvents renderer =
          do renderer^.rRefExport.reGetKeyboardHandler.kbdUpdate
             time <- Timer.milliseconds
             globals.gSysFrameTime .= time