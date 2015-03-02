{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Quake (quake) where

import Control.Applicative
import Control.Monad.State (MonadState, StateT, runStateT)

import QuakeState

type QBasePath = FilePath

newtype Quake a = Quake (StateT QuakeState IO a)
                    deriving (Functor, Applicative, Monad, MonadState QuakeState)

runQuake :: QuakeState -> Quake a -> IO (a, QuakeState)
runQuake qs (Quake q) = runStateT q qs

quake :: QBasePath -> IO ()
quake qbasePath = do
    runQuake undefined $ do
      undefined
    return ()
