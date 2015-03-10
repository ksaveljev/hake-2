{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Internal where

import Data.Sequence (Seq)
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Control.Lens (Lens)
import qualified Control.Monad.State.Lazy as Lazy
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Game.CVarT
import QCommon.SizeBufT
import QCommon.FileLinkT
import QCommon.SearchPathT

newtype Quake a = Quake (StateT QuakeState (ExceptT B.ByteString IO) a)
                    deriving (Functor, Applicative, Monad, MonadIO, MonadError B.ByteString, MonadState QuakeState)

type XCommandT = Quake ()

data QuakeState =
  QuakeState { _globals     :: Globals
             , _comGlobals  :: ComGlobals
             , _cmdGlobals  :: CmdGlobals
             , _keyGlobals  :: KeyGlobals
             , _cvarGlobals :: CVarGlobals
             , _fsGlobals   :: FSGlobals
             }

data Globals =
  Globals { _curtime            :: Int
          , _cmdWait            :: Bool

          , _aliasCount         :: Int
          , _cTraces            :: Int
          , _cBrushTraces       :: Int
          , _cPointContents     :: Int
          , _serverState        :: Int

          , _cmdText            :: SizeBufT
          , _cmdTextBuf         :: B.ByteString

          , _cvarVars           :: Seq CVarT

          , _keyBindings        :: V.Vector (Maybe B.ByteString)
          , _keyDown            :: UV.Vector Bool
          , _chatTeam           :: Bool
          , _chatBuffer         :: B.ByteString
          , _keyLines           :: V.Vector B.ByteString
          , _keyLinePos         :: Int
          , _editLine           :: Int
          }

data CVarGlobals =
  CVarGlobals { _clAddBlend         :: CVarT
              , _clAddEntities      :: CVarT
              , _clAddLights        :: CVarT
              , _clAddParticles     :: CVarT
              , _clAngleSpeedKey    :: CVarT
              , _clAutoSkins        :: CVarT
              , _clFootSteps        :: CVarT
              , _clForwardSpeed     :: CVarT
              , _clGun              :: CVarT
              , _clMaxFPS           :: CVarT
              , _clNoSkins          :: CVarT
              , _clPitchSpeed       :: CVarT
              , _clPredict          :: CVarT
              , _clRun              :: CVarT
              , _clSideSpeed        :: CVarT
              , _clStereo           :: CVarT
              , _clStereoSeparation :: CVarT
              , _clTimeDemo         :: CVarT
              , _clTimeout          :: CVarT
              , _clUpSpeed          :: CVarT
              , _clYawSpeed         :: CVarT
              , _dedicated          :: CVarT
              , _developer          :: CVarT
              , _fixedTime          :: CVarT
              , _freeLook           :: CVarT
              , _hostSpeeds         :: CVarT
              , _logStats           :: CVarT
              , _logfileActive      :: CVarT
              , _lookSpring         :: CVarT
              , _lookStrafe         :: CVarT
              , _nostdout           :: CVarT
              , _sensitivity        :: CVarT
              , _showTrace          :: CVarT
              , _timeScale          :: CVarT
              , _inMouse            :: CVarT
              , _inJoystick         :: CVarT
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
             , _cgCmdArgs      :: B.ByteString
             }

data KeyGlobals =
  KeyGlobals { _anyKeyDown  :: Int
             , _keyWaiting  :: Int
             , _historyLine :: Int
             , _shiftDown   :: Bool
             , _keyRepeats  :: UV.Vector Int
             , _menuBound   :: UV.Vector Bool
             , _consoleKeys :: UV.Vector Bool
             , _keyNames    :: V.Vector (Maybe B.ByteString)
             }

data FSGlobals =
  FSGlobals { _fsGameDir         :: B.ByteString
            , _fsUserDir         :: B.ByteString
            , _fsBaseDir         :: CVarT
            , _fsCDDir           :: CVarT
            , _fsGameDirVar      :: CVarT
            , _fsLinks           :: Seq FileLinkT
            , _fsSearchPaths     :: [SearchPathT]
            , _fsBaseSearchPaths :: [SearchPathT]
            }

data CmdFunctionT =
  CmdFunctionT { _cfName     :: B.ByteString
               , _cfFunction :: XCommandT
               }

type SizeBufTLens = Lens QuakeState QuakeState SizeBufT SizeBufT
