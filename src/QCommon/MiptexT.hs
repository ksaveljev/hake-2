module QCommon.MiptexT where

import qualified Data.Vector.Unboxed as UV
import qualified Data.ByteString as B

data MiptexT =
  MiptexT { mName      :: B.ByteString
          , mWidth     :: Int
          , mHeight    :: Int
          , mOffsets   :: UV.Vector Int
          , mAnimFrame :: B.ByteString
          , mFlags     :: Int
          , mContents  :: Int
          , mValue     :: Int
          }
