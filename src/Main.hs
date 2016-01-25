{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens ((^.), (.=))
import Control.Monad.State (when, liftM, void)
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Random (newStdGen)

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.QCommon as QCommon
import qualified Sys.Timer as Timer

isDedicatedCmdArg :: [String] -> Bool
isDedicatedCmdArg ("+set":"dedicated":"1":_) = True
isDedicatedCmdArg (_:xs) = isDedicatedCmdArg xs
isDedicatedCmdArg [] = False

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    runQuake initialQuakeState $ do
      io newStdGen >>= \g -> globals.rnd .= g

      args <- io getArgs
      let dedicatedFlag = isDedicatedCmdArg args

      Just dedicated <- CVar.get "dedicated" "0" Constants.cvarNoSet

      when dedicatedFlag $ do
        Com.printf "Starting in dedicated mode.\n"
        CVar.update dedicated { _cvValue = 1.0 }

      -- in C the first arg is the filename
      let updatedArgs = "hake2" : args

      QCommon.init updatedArgs

      void $ CVar.get "nostdout" "0" 0

      startTime <- Timer.milliseconds
      mainLoop startTime

    return ()

  where mainLoop oldTime = do
          newTime <- Timer.milliseconds
          let time = newTime - oldTime

          when (time > 0) $ QCommon.frame time

          mainLoop newTime
