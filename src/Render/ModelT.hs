{-# LANGUAGE TemplateHaskell #-}
module Render.ModelT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)
import Data.Sequence (Seq)
import qualified Data.ByteString as B

import Game.CPlaneT
import Render.ImageT
import Render.MLeafT
import Render.MModelT
import Render.MSurfaceT

data ModelT =
  ModelT { _mName                 :: B.ByteString
         , _mRegistrationSequence :: Int
         , _mType                 :: Int
         , _mNumFrames            :: Int
         , _mFlags                :: Int
         , _mMins                 :: V3 Float
         , _mMaxs                 :: V3 Float
         , _mRadius               :: Float
         , _mClipBox              :: Bool
         , _mClipMins             :: V3 Float
         , _mClipMaxs             :: V3 Float
         , _mFirstModelSurface    :: Int
         , _mNumModelSurfaces     :: Int
         , _mLightmap             :: Int
         , _mNumSubModels         :: Int
         , _mSubModels            :: Seq MModelT
         , _mNumPlanes            :: Int
         , _mPlanes               :: Seq CPlaneT
         , _mNumLeafs             :: Int
         , _mLeafs                :: Seq MLeafT
         , _mNumVertexes          :: Int
         , _mVertexes             :: Seq MVertexT
         , _mNumEdges             :: Int
         , _mEdges                :: Seq MEdgeT
         , _mNumNodes             :: Int
         , _mFirstNode            :: Int
         , _mNodes                :: Seq MNodeT
         , _mNumTexInfo           :: Int
         , _mTexInfo              :: Seq MTexInfoT
         , _mNumSurfaces          :: Int
         , _mSurfaces             :: Seq MSurfaceT
         , _mNumSurfEdges         :: Int
         , _mSurfEdges            :: Seq Int
         , _mNumMarkSurfaces      :: Int
         , _mMarkSurfaces         :: Seq MSurfaceT
         -- TODO: qfiles.dvis_t vis
         , _mLightdata            :: B.ByteString
         , _mSkins                :: Seq ImageT
         , _mExtraDataSize        :: Int
         , _mExtraData            :: Maybe B.ByteString
         }

makeLenses ''ModelT
