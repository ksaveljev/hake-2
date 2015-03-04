{-# LANGUAGE TemplateHaskell #-}
module QCommon.MiptexT where

import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as B

data MiptexT =
  MiptexT { _mName      :: B.ByteString
          , _mWidth     :: Int
          , _mHeight    :: Int
          , _mOffsets   :: UV.Vector Int
          , _mAnimFrame :: B.ByteString
          , _mFlags     :: Int
          , _mContents  :: Int
          , _mValue     :: Int
          }

makeLenses ''MiptexT
