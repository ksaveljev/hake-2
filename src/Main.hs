{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens ((.=), use)
import Control.Monad.State (when, liftM, void)
import System.Environment (getArgs)

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.CVar as CVar
import qualified QCommon.QCommon as QCommon
import qualified Sys.Timer as Timer

isDedicatedCmdArg :: [String] -> Bool
isDedicatedCmdArg ("+set":"dedicated":"1":_) = True
isDedicatedCmdArg (_:xs) = isDedicatedCmdArg xs
isDedicatedCmdArg [] = False

main :: IO ()
main = do
    runQuake initialQuakeState $ do
      args <- io getArgs
      let dedicatedFlag = isDedicatedCmdArg args

      void $ CVar.getAndSet "dedicated" "0" Constants.cvarNoSet (cvarGlobals.dedicated)

      when dedicatedFlag $ cvarGlobals.dedicated.cvValue .= 1.0

      -- if (globals.dedicated.cvValue != 1.0)
      whenQ (liftM (/= 1.0) (use $ cvarGlobals.dedicated.cvValue)) $ do
        undefined -- TODO: init our client window

      -- in C the first arg is the filename
      let updatedArgs = "hake2" : args

      QCommon.init updatedArgs

      void $ CVar.getAndSet "nostdout" "0" 0 (cvarGlobals.nostdout)

      startTime <- Timer.milliseconds
      mainLoop startTime

    return ()

  where mainLoop oldTime = do
          newTime <- Timer.milliseconds
          let time = newTime - oldTime

          when (time > 0) $ QCommon.frame time

          mainLoop newTime
