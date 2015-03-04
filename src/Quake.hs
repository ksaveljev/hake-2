{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Quake ( Quake
             , quake
             , io
             ) where

import Control.Applicative
import Control.Monad.State
import System.Environment (getArgs)

import QuakeState

newtype Quake a = Quake (StateT QuakeState IO a)
                    deriving (Functor, Applicative, Monad, MonadIO, MonadState QuakeState)

runQuake :: QuakeState -> Quake a -> IO (a, QuakeState)
runQuake qs (Quake q) = runStateT q qs

io :: MonadIO m => IO a -> m a
io = liftIO

quake :: IO ()
quake = do
    runQuake undefined $ do
      args <- io getArgs
      let dedicated = isDedicatedCmdArg args
      -- check if we start in dedicated mode
      -- set dedicated value
      -- if not dedicated then init our client window
      -- strip some args and call QCommon.init
      -- grab current time
      -- forever loop calling QCommon.frame
      undefined
    return ()

isDedicatedCmdArg :: [String] -> Bool
isDedicatedCmdArg ("+set":"dedicated":"1":_) = True
isDedicatedCmdArg (_:xs) = isDedicatedCmdArg xs
isDedicatedCmdArg [] = False
