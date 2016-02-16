{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
module Types where

import           Control.Monad.State (State,StateT,MonadState,MonadIO,lift,get,put,liftIO)
import           Control.Monad.Coroutine (Coroutine(..), suspend)
import qualified Data.ByteString as B
import qualified Data.HashMap.Lazy as HM
import           Data.Sequence (Seq)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           System.IO (Handle)
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
  , _cmdGlobals :: CmdGlobals
  , _keyGlobals :: KeyGlobals
  , _fsGlobals  :: FSGlobals
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
  { _gCurTime    :: Int
  , _gCmdWait    :: Bool
  , _gAliasCount :: Int
  , _gCmdText    :: SizeBufT
  , _gCVars      :: HM.HashMap B.ByteString CVarT
  , _gKeyLines   :: V.Vector B.ByteString
  , _gKeyLinePos :: Int
  , _gRnd        :: StdGen
  }

data ComGlobals = ComGlobals
  { _cgComArgc :: Int
  , _cgComArgv :: V.Vector B.ByteString
  }

data CmdGlobals = CmdGlobals
  { _cgCmdFunctions :: Seq CmdFunctionT
  , _cgCmdArgc      :: Int
  , _cgCmdArgv      :: V.Vector B.ByteString
  , _cgCmdArgs      :: B.ByteString
  }
  
data KeyGlobals = KeyGlobals
  { _kgAnyKeyDown  :: Int
  , _kgKeyWaiting  :: Int
  , _kgHistoryLine :: Int
  , _kgShiftDown   :: Bool
  , _kgKeyRepeats  :: UV.Vector Int
  , _kgMenuBound   :: UV.Vector Bool
  , _kgConsoleKeys :: UV.Vector Bool
  , _kgKeyNames    :: V.Vector (Maybe B.ByteString)
  }

data FSGlobals = FSGlobals
  { _fsGameDir         :: B.ByteString
  , _fsUserDir         :: B.ByteString
  , _fsLinks           :: Seq FileLinkT
  , _fsSearchPaths     :: [SearchPathT]
  , _fsBaseSearchPaths :: [SearchPathT]
  , _fsFileFromPak     :: !Int
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

data CmdFunctionT = CmdFunctionT
  { _cfName     :: B.ByteString
  , _cfFunction :: Maybe XCommandT
  }

data SearchPathT = SearchPathT
  { _spFilename :: B.ByteString
  , _spPack     :: Maybe PackT
  } deriving (Eq)

data PackT = PackT
  { _pFilename   :: B.ByteString
  , _pHandle     :: Maybe Handle
  , _pBackBuffer :: B.ByteString
  , _pNumFiles   :: Int
  , _pFiles      :: HM.HashMap B.ByteString PackFileT
  } deriving (Eq)

data PackFileT = PackFileT
  { _pfName    :: B.ByteString
  , _pfFilePos :: Int
  , _pfFileLen :: Int
  } deriving (Eq)

data FileLinkT = FileLinkT
  { _flFrom       :: B.ByteString
  , _flFromLength :: Int
  , _flTo         :: B.ByteString
  }