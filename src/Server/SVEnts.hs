module Server.SVEnts
    ( recordDemoMessage
    ) where

import           Control.Lens         (use)

import           QuakeState
import           Server.ServerStaticT
import           Types

recordDemoMessage :: Quake ()
recordDemoMessage = do
    demoFile <- use (svGlobals.svServerStatic.ssDemoFile)
    maybe (return ()) doRecordDemoMessage demoFile
  where
    doRecordDemoMessage demoFile = error "SVEnts.recordDemoMessage" -- TODO