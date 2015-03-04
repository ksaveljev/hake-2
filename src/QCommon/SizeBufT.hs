{-# LANGUAGE TemplateHaskell #-}
module QCommon.SizeBufT where

import Data.Word (Word8)
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

data SizeBufT =
  SizeBufT { _sizeBufAllowOverflow :: Bool
           , _sizeBufOverflowed    :: Bool
           , _sizeBufData          :: UV.Vector Word8
           , _sizeBufMaxSize       :: Int
           , _sizeBufCurSize       :: Int
           , _sizeBufReadCount     :: Int
           }

makeLenses ''SizeBufT
