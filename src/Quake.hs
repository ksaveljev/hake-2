module Quake where

import           Control.Lens         ((.=), (&), (.~))
import           Control.Monad.Except (runExceptT)
import           Control.Monad        (when, void)
import           Control.Monad.State  (runStateT)
import           System.Environment   (getArgs)
import           System.Random        (newStdGen)

import qualified Constants
import           Game.CVarT
import qualified QCommon.Com         as Com
import qualified QCommon.CVar        as CVar
import qualified QCommon.QCommon     as QCommon
import           QuakeState
import qualified Sys.Timer           as Timer
import           Types

runQuake :: IO ()
runQuake = void $ runExceptT $ runStateT (unQuake quake) initialQuakeState

quake :: Quake ()
quake = do
    args <- io getArgs
    initializeStdGen
    checkDedicatedMode (isDedicatedCmdArg args)
    QCommon.initialize ("hake2" : args) -- in C the first arg is the filename 
    void (CVar.get "nostdout" "0" 0)
    startTime <- Timer.milliseconds
    mainLoop startTime
  where
    initializeStdGen = do
        stdGen <- io newStdGen
        globals.gRnd .= stdGen
    mainLoop oldTime = do
        newTime <- Timer.milliseconds
        let time = newTime - oldTime
        when (time > 0) (QCommon.frame time)
        mainLoop newTime
             
checkDedicatedMode :: Bool -> Quake ()
checkDedicatedMode dedicatedFlag = do
    dedicated <- CVar.get "dedicated" "0" Constants.cvarNoSet
    when dedicatedFlag $
        maybe (Com.fatalError "dedicated cvar not set")
              (\cvar -> do Com.printf "Starting in dedicated mode.\n"
                           CVar.update (cvar & cvValue .~ 1.0))
              dedicated

isDedicatedCmdArg :: [String] -> Bool
isDedicatedCmdArg ("+set":"dedicated":"1":_) = True
isDedicatedCmdArg (_:xs) = isDedicatedCmdArg xs
isDedicatedCmdArg _ = False