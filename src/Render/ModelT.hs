{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Render.ModelT ( ModelT(..)
                     , module Render.ModelT
                     , module Game.CPlaneT
                     , module QCommon.QFiles.BSP.DVisT
                     , module Render.MEdgeT
                     , module Render.MLeafT
                     , module Render.MModelT
                     , module Render.MNodeT
                     , module Render.MSurfaceT
                     , module Render.MTexInfoT
                     , module Render.MVertexT
                     ) where

import Control.Lens (makeLenses)
import Linear (V3(..))
import qualified Data.Vector as V

import Internal
import Game.CPlaneT
import QCommon.QFiles.BSP.DVisT
import Render.MEdgeT
import Render.MLeafT
import Render.MModelT
import Render.MNodeT
import Render.MSurfaceT
import Render.MTexInfoT
import Render.MVertexT

makeLenses ''ModelT

newModelT :: ModelT
newModelT =
  ModelT { _mName                 = ""
         , _mRegistrationSequence = 0
         , _mType                 = 0
         , _mNumFrames            = 0
         , _mFlags                = 0
         , _mMins                 = V3 0 0 0
         , _mMaxs                 = V3 0 0 0
         , _mRadius               = 0
         , _mClipBox              = False
         , _mClipMins             = V3 0 0 0
         , _mClipMaxs             = V3 0 0 0
         , _mFirstModelSurface    = 0
         , _mNumModelSurfaces     = 0
         , _mLightmap             = 0
         , _mNumSubModels         = 0
         , _mSubModels            = V.empty
         , _mNumPlanes            = 0
         , _mPlanes               = V.empty
         , _mNumLeafs             = 0
         , _mLeafs                = V.empty
         , _mNumVertexes          = 0
         , _mVertexes             = V.empty
         , _mNumEdges             = 0
         , _mEdges                = V.empty
         , _mNumNodes             = 0
         , _mFirstNode            = 0
         , _mNodes                = V.empty
         , _mNumTexInfo           = 0
         , _mTexInfo              = V.empty
         , _mNumSurfaces          = 0
         , _mSurfaces             = V.empty
         , _mNumSurfEdges         = 0
         , _mSurfEdges            = V.empty
         , _mNumMarkSurfaces      = 0
         , _mMarkSurfaces         = V.empty
         , _mVis                  = Nothing
         , _mLightdata            = Nothing
         , _mSkins                = V.empty
         , _mExtraDataSize        = 0
         , _mExtraData            = Nothing
         }
