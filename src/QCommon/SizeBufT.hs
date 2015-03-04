{-# LANGUAGE TemplateHaskell #-}
module QCommon.SizeBufT where

import Data.Word (Word8)
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

data SizeBufT =
  SizeBufT { _sbAllowOverflow :: Bool
           , _sbOverflowed    :: Bool
           , _sbData          :: UV.Vector Word8
           , _sbMaxSize       :: Int
           , _sbCurSize       :: Int
           , _sbReadCount     :: Int
           }

makeLenses ''SizeBufT
