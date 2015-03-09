{-# LANGUAGE TemplateHaskell #-}
module CVarGlobals ( module CVarGlobals
                   , module Game.CVarT
                   ) where

import Control.Lens (makeLenses)

import Internal
import Game.CVarT

makeLenses ''CVarGlobals

initialCVarGlobals :: CVarGlobals
initialCVarGlobals =
  CVarGlobals { _clAddBlend         = newCVarT
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
              }
