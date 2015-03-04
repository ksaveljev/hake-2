{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Control.Monad.State
import System.Environment (getArgs)

import Quake
import QuakeState
import qualified QCommon.CVar as CVar
import qualified QCommon.QCommon as QCommon
import qualified Sys.Timer as Timer

isDedicatedCmdArg :: [String] -> Bool
isDedicatedCmdArg ("+set":"dedicated":"1":_) = True
isDedicatedCmdArg (_:xs) = isDedicatedCmdArg xs
isDedicatedCmdArg [] = False

main :: IO ()
main = do
    runQuake defaultQuakeState $ do
      args <- io getArgs
      let dedicatedFlag = isDedicatedCmdArg args

      dedicatedCVar <- CVar.get "dedicated" "0" 1 -- TODO: last param
      globals.dedicated .= dedicatedCVar

      when dedicatedFlag $ globals.dedicated.cvValue .= 1.0

      -- if (globals.dedicated.cvValue != 1.0)
      whenQ (liftM ((/= 1.0) . (^.globals.dedicated.cvValue)) get) $ do
        undefined -- TODO: init our client window

      -- TODO: strip some args and call QCommon.init
      QCommon.init undefined -- TODO

      nostdoutCVar <- CVar.get "nostdout" "0" 0
      globals.nostdout .= nostdoutCVar

      startTime <- Timer.milliseconds
      mainLoop startTime

    return ()

  where mainLoop oldTime = do
          newTime <- Timer.milliseconds
          let time = newTime - oldTime

          when (time > 0) $ QCommon.frame time

          mainLoop newTime
