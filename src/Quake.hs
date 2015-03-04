{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Quake ( Quake
             , runQuake
             , io
             , whenQ
             ) where

import Control.Applicative
import Control.Monad.State

import QuakeState

newtype Quake a = Quake (StateT QuakeState IO a)
                    deriving (Functor, Applicative, Monad, MonadIO, MonadState QuakeState)

runQuake :: QuakeState -> Quake a -> IO (a, QuakeState)
runQuake qs (Quake q) = runStateT q qs

io :: MonadIO m => IO a -> m a
io = liftIO

whenQ :: Quake Bool -> Quake () -> Quake ()
whenQ q f = q >>= \b -> when b f
