{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Globals ( module Globals
               , module Game.CVarT
               , module QCommon.SizeBufT
               ) where

import Control.Lens (makeLenses)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Game.CVarT
import QCommon.SizeBufT

import Internal

makeLenses ''Globals

initialGlobals :: Globals
initialGlobals =
  Globals { _curtime            = 0
          , _cmdWait            = False

          , _aliasCount         = 0
          , _cTraces            = 0
          , _cBrushTraces       = 0
          , _cPointContents     = 0
          , _serverState        = 0

          , _clAddBlend         = newCVarT
          , _clAddEntities      = newCVarT
          , _clAddLights        = newCVarT
          , _clAddParticles     = newCVarT
          , _clAngleSpeedKey    = newCVarT
          , _clAutoSkins        = newCVarT
          , _clFootSteps        = newCVarT
          , _clForwardSpeed     = newCVarT
          , _clGun              = newCVarT
          , _clMaxFPS           = newCVarT
          , _clNoSkins          = newCVarT
          , _clPitchSpeed       = newCVarT
          , _clPredict          = newCVarT
          , _clRun              = newCVarT
          , _clSideSpeed        = newCVarT
          , _clStereo           = newCVarT
          , _clStereoSeparation = newCVarT
          , _clTimeDemo         = newCVarT
          , _clTimeout          = newCVarT
          , _clUpSpeed          = newCVarT
          , _clYawSpeed         = newCVarT
          , _dedicated          = newCVarT
          , _developer          = newCVarT
          , _fixedTime          = newCVarT
          , _freeLook           = newCVarT
          , _hostSpeeds         = newCVarT
          , _logStats           = newCVarT
          , _logfileActive      = newCVarT
          , _lookSpring         = newCVarT
          , _lookStrafe         = newCVarT
          , _nostdout           = newCVarT
          , _sensitivity        = newCVarT
          , _showTrace          = newCVarT
          , _timeScale          = newCVarT
          , _inMouse            = newCVarT
          , _inJoystick         = newCVarT
          
          , _cmdText            = SizeBufT False False "" 0 0 0
          , _cmdTextBuf         = ""

          , _cvarVars           = Seq.empty

          , _keyBindings        = V.replicate 256 Nothing
          , _keyDown            = UV.empty
          , _chatTeam           = False
          , _chatBuffer         = ""
          , _keyLines           = V.empty
          , _keyLinePos         = 0
          , _editLine           = 0
          }
