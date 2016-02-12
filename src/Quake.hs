module Quake
  (quake)
  where

import qualified Constants
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.QCommon as QCommon
import           QuakeState
import           Types

import Control.Lens ((.=),(&),(.~))
import Control.Monad (when,void)
import Control.Monad.Trans (lift)
import Control.Monad.Coroutine.SuspensionFunctors (request)
import System.Random (StdGen)

quake :: [String] -> StdGen -> Quake Int
quake args stdGen =
  do globals .= 3 -- TODO
     let dedicatedFlag = isDedicatedCmdArg args
         -- in C the first arg is the filename
         updatedArgs = "hake2" : args
     dedicated <- CVar.getExisting "dedicated" "0" Constants.cvarNoSet
     when dedicatedFlag
          (do Com.printf "Starting in dedicated mode.\n"
              CVar.update (dedicated & cvValue .~ 1.0))
     void (CVar.get "nostdout" "0" 0)
     startTime <- request 3 -- TODO
     mainLoop startTime
  where mainLoop oldTime =
          do newTime <- request 3 -- TODO
             let time = newTime - oldTime
             when (time > 0) (QCommon.frame time)
             mainLoop newTime

isDedicatedCmdArg :: [String] -> Bool
isDedicatedCmdArg ("+set":"dedicated":"1":_) = True
isDedicatedCmdArg (_:xs) = isDedicatedCmdArg xs
isDedicatedCmdArg _ = False
