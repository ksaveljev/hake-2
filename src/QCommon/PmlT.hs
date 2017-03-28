{-# LANGUAGE TemplateHaskell #-}
module QCommon.PmlT ( module QCommon.PmlT
                    , module Game.CSurfaceT
                    ) where

import Control.Lens (makeLenses)
import Linear.V3 (V3(..))

import Game.CSurfaceT

data PmlT =
  PmlT { _pmlOrigin         :: V3 Float
       , _pmlVelocity       :: V3 Float
       , _pmlForward        :: V3 Float
       , _pmlRight          :: V3 Float
       , _pmlUp             :: V3 Float
       , _pmlFrameTime      :: Float
       , _pmlGroundSurface  :: Maybe CSurfaceT
       , _pmlGroundContents :: Int
       , _pmlPreviousOrigin :: V3 Float
       , _pmlLadder         :: Bool
       }

makeLenses ''PmlT

newPmlT :: PmlT
newPmlT =
  PmlT { _pmlOrigin         = V3 0 0 0
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
