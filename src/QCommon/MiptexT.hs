{-# LANGUAGE TemplateHaskell #-}
module QCommon.MiptexT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Util.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as UV

mipLevels :: Int
mipLevels = 4

miptexNameSize :: Int
miptexNameSize = 32

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

newMiptexT :: BL.ByteString -> MiptexT
newMiptexT = runGet getMiptexT

getMiptexT :: Get MiptexT
getMiptexT = MiptexT <$> (B.takeWhile (/= 0) <$> getByteString 32) -- trim name
                     <*> getInt
                     <*> getInt
                     <*> getOffsets
                     <*> (B.takeWhile (/= 0) <$> getByteString 32) -- trim animFrame
                     <*> getInt
                     <*> getInt
                     <*> getInt

  where getOffsets :: Get (UV.Vector Int)
        getOffsets = UV.replicateM 4 getInt
