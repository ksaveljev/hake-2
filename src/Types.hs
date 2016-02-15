{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Types where

import           Control.Monad.State (State,StateT,MonadState,MonadIO,lift,get,put,liftIO)
import           Control.Monad.Coroutine (Coroutine(..), suspend)
import qualified Data.ByteString as B
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import           System.Random (StdGen)

type Quake = Coroutine IORequest (State QuakeState)

instance MonadState QuakeState Quake where
  get = lift get
  put = lift . put

type QuakeIO = StateT QuakeIOState IO

io :: MonadIO m => IO a -> m a
io = liftIO

data QuakeState = QuakeState
  { _globals    :: Globals
  , _comGlobals :: ComGlobals
  }

data QuakeIOState = QuakeIOState
  { _pf :: Int }

data IORequest x
  = forall a. RunIO (QuakeIO a) (a -> x)

instance Functor IORequest where
  fmap f (RunIO x g) = RunIO x (f . g)

request :: Monad m => QuakeIO a -> Coroutine IORequest m a
request x = suspend (RunIO x return)

data XCommandT = XCommandT
  { _xcName :: B.ByteString
  , _xcCmd  :: Quake ()
  }

instance Eq XCommandT where
  x == y = _xcName x == _xcName y

data Globals = Globals
  { _gCurTime :: Int
  , _gCmdText :: SizeBufT
  , _gCVars   :: HM.HashMap B.ByteString CVarT
  , _gRnd     :: StdGen
  }

data ComGlobals = ComGlobals
  { _cgComArgc :: Int
  , _cgComArgv :: V.Vector B.ByteString
  }
  
data CVarT = CVarT
  { _cvName          :: B.ByteString
  , _cvString        :: B.ByteString
  , _cvLatchedString :: Maybe B.ByteString
  , _cvFlags         :: Int
  , _cvModified      :: Bool
  , _cvValue         :: Float
  } deriving (Eq)

data SizeBufT = SizeBufT
  { _sbAllowOverflow :: Bool
  , _sbOverflowed    :: Bool
  , _sbData          :: B.ByteString
  , _sbMaxSize       :: Int
  , _sbCurSize       :: Int
  , _sbReadCount     :: Int
  }