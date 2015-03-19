module Quake ( Quake
             , runQuake
             , io
             , whenQ
             ) where

import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.ByteString as B

import Internal

runQuake :: QuakeState -> Quake a -> IO (Either B.ByteString (a, QuakeState))
runQuake qs (Quake q) = runExceptT $ runStateT q qs

io :: MonadIO m => IO a -> m a
io = liftIO

whenQ :: Quake Bool -> Quake () -> Quake ()
whenQ q f = q >>= \b -> when b f
