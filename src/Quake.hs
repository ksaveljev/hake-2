{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Quake ( Quake
             , runQuake
             , io
             , whenQ
             ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import qualified Data.ByteString as B

import QuakeState

newtype Quake a = Quake (StateT QuakeState (ExceptT B.ByteString IO) a)
                    deriving (Functor, Applicative, Monad, MonadIO, MonadError B.ByteString, MonadState QuakeState)

runQuake :: QuakeState -> Quake a -> IO (Either B.ByteString (a, QuakeState))
runQuake qs (Quake q) = runExceptT $ runStateT q qs

io :: MonadIO m => IO a -> m a
io = liftIO

whenQ :: Quake Bool -> Quake () -> Quake ()
whenQ q f = q >>= \b -> when b f
