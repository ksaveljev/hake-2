module Quake
  (quake)
  where

import qualified Constants
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.QCommon as QCommon
import           QuakeState
import qualified Sys.Timer as Timer
import           Types

import           Control.Lens ((.=),(&),(.~))
import           Control.Monad (when,void)
import           System.Environment (getArgs)

quake :: Quake Int
quake =
  do args <- request (io getArgs)
     globals .= 3 -- TODO
     let dedicatedFlag = isDedicatedCmdArg args
         -- in C the first arg is the filename
         updatedArgs = "hake2" : args
     dedicated <- CVar.getExisting "dedicated" "0" Constants.cvarNoSet
     when dedicatedFlag
          (do Com.printf "Starting in dedicated mode.\n"
              CVar.update (dedicated & cvValue .~ 1.0))
     QCommon.initialize updatedArgs
     void (CVar.get "nostdout" "0" 0)
     startTime <- Timer.milliseconds
     mainLoop startTime
  where mainLoop oldTime =
          do newTime <- Timer.milliseconds
             let time = newTime - oldTime
             when (time > 0) (QCommon.frame time)
             mainLoop newTime

isDedicatedCmdArg :: [String] -> Bool
isDedicatedCmdArg ("+set":"dedicated":"1":_) = True
isDedicatedCmdArg (_:xs) = isDedicatedCmdArg xs
isDedicatedCmdArg _ = False
