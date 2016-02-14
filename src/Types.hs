{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Types where

import           Control.Monad.State (State,StateT,MonadState,MonadIO,lift,get,put,liftIO)
import           Control.Monad.Coroutine (Coroutine(..), suspend)
import qualified Data.ByteString as B

type Quake = Coroutine IORequest (State QuakeState)

instance MonadState QuakeState Quake where
  get = lift get
  put = lift . put

type QuakeIO = StateT QuakeIOState IO

io :: MonadIO m => IO a -> m a
io = liftIO

data QuakeState =
  QuakeState { _globals :: Int }

data QuakeIOState =
  QuakeIOState { _pf :: Int }

data IORequest x
  = forall a. RunIO (QuakeIO a) (a -> x)

instance Functor IORequest where
  fmap f (RunIO x g) = RunIO x (f . g)

request :: Monad m => QuakeIO a -> Coroutine IORequest m a
request x = suspend (RunIO x return)

data CVarT = CVarT
  { _cvName          :: B.ByteString
  , _cvString        :: B.ByteString
  , _cvLatchedString :: Maybe B.ByteString
  , _cvFlags         :: Int
  , _cvModified      :: Bool
  , _cvValue         :: Float
  } deriving (Eq)
