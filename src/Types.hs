{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Types where

import           Control.Monad.State (State,StateT,MonadState,MonadIO,lift,get,put)
import           Control.Monad.Coroutine (Coroutine(..))
import           Control.Monad.Coroutine.SuspensionFunctors (Request)
import qualified Data.ByteString as B

type Quake = Coroutine (Request Int Int) (State QuakeState)

instance MonadState QuakeState Quake where
  get = lift get
  put = lift . put

newtype QuakeIOS s a =
  QuakeIO { unQuakeIO :: (StateT s IO) a }
  deriving (Functor,Applicative,Monad,MonadIO,MonadState s)

type QuakeIO = QuakeIOS QuakeIOState

data QuakeState =
  QuakeState { _globals :: Int }

data QuakeIOState =
  QuakeIOState { _pf :: Int }

data CVarT = CVarT
  { _cvName          :: B.ByteString
  , _cvString        :: B.ByteString
  , _cvLatchedString :: Maybe B.ByteString
  , _cvFlags         :: Int
  , _cvModified      :: Bool
  , _cvValue         :: Float
  } deriving (Eq)
