module Server.SVEnts
  ( recordDemoMessage
  ) where

import QuakeState
import Server.ServerStaticT
import Types

import Control.Lens (use)

recordDemoMessage :: Quake ()
recordDemoMessage =
  do demoFile <- use (svGlobals.svServerStatic.ssDemoFile)
     maybe (return ()) doRecordDemoMessage demoFile
  where doRecordDemoMessage demoFile = error "SVEnts.recordDemoMessage" -- TODO