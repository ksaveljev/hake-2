{-# LANGUAGE TemplateHaskell #-}
module QCommon.PmlT where

import           Control.Lens (makeLenses)
import           Linear.V3    (V3(..))

import           Types

makeLenses ''PmlT

newPmlT :: PmlT
newPmlT = PmlT
    { _pmlOrigin         = V3 0 0 0
    , _pmlVelocity       = V3 0 0 0
    , _pmlForward        = V3 0 0 0
    , _pmlRight          = V3 0 0 0
    , _pmlUp             = V3 0 0 0
    , _pmlFrameTime      = 0
    , _pmlGroundSurface  = Nothing
    , _pmlGroundContents = 0
    , _pmlPreviousOrigin = V3 0 0 0
    , _pmlLadder         = False
    }