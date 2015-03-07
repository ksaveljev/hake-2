{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Internal where

import Data.Word (Word8)
import Data.Sequence (Seq)
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import qualified Control.Monad.State.Lazy as Lazy
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Game.CVarT
import QCommon.SizeBufT

newtype Quake a = Quake (StateT QuakeState (ExceptT B.ByteString IO) a)
                    deriving (Functor, Applicative, Monad, MonadIO, MonadError B.ByteString, MonadState QuakeState)

type XCommandT = Quake ()

data QuakeState =
  QuakeState { _globals    :: Globals
             , _comGlobals :: ComGlobals
             , _cmdGlobals :: CmdGlobals
             , _keyGlobals :: KeyGlobals
             }

data Globals =
  Globals { _curtime     :: Int
          , _cmdWait     :: Bool
          , _dedicated   :: CVarT
          , _nostdout    :: CVarT

          , _cmdText     :: SizeBufT
          , _cmdTextBuf  :: UV.Vector Word8

          , _cvarVars    :: Seq CVarT

          , _keyBindings :: V.Vector (Maybe B.ByteString)
          , _keyDown     :: UV.Vector Bool
          , _chatTeam    :: Bool
          , _chatBuffer  :: B.ByteString
          , _keyLines    :: V.Vector B.ByteString
          , _keyLinePos  :: Int
          , _editLine    :: Int
          }

data ComGlobals =
  ComGlobals { _cgComArgc   :: Int
             , _cgComArgv   :: V.Vector B.ByteString
             , _cgRecursive :: Bool
             , _cgMsg       :: B.ByteString
             }

data CmdGlobals =
  CmdGlobals { _cgCmdFunctions :: Seq CmdFunctionT
             , _cgCmdArgc      :: Int
             , _cgCmdArgv      :: V.Vector B.ByteString
             }

data CmdFunctionT =
  CmdFunctionT { _cfName     :: B.ByteString
               , _cfFunction :: XCommandT
               }

data KeyGlobals =
  KeyGlobals { _anyKeyDown  :: Int
             , _keyWaiting  :: Int
             , _historyLine :: Int
             , _shiftDown   :: Bool
             , _keyRepeats  :: UV.Vector Int
             , _menuBound   :: UV.Vector Bool
             , _consoleKeys :: UV.Vector Bool
             , _keyNames    :: V.Vector B.ByteString
             }
