module Sys.Timer where

import           Control.Lens          (use)
import           Data.IORef            (readIORef, writeIORef)
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           QuakeState
import           Types

milliseconds :: Quake Int
milliseconds = do
    curTime <- use (globals.gCurTime)
    io $ do
        t <- round . (* 1000) <$> getPOSIXTime
        curTime `writeIORef` t
        return t

getCurTime :: Quake Int
getCurTime = use (globals.gCurTime) >>= io . readIORef