module Sys.Timer where

import QuakeIOState
import Types

import Control.Lens (use)
import Data.IORef (readIORef, writeIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)

milliseconds :: Quake Int
milliseconds =
  do curTime <- request (use gCurTime)
     request (io $
       do t <- round . (* 1000) <$> getPOSIXTime
          curTime `writeIORef` t
          return t)

getCurTime :: Quake Int
getCurTime = request (use gCurTime >>= io . readIORef)