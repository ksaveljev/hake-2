module QCommon.SizeBufT where

import Data.Int (Int8)
import qualified Data.Vector.Unboxed as UV

data SizeBufT =
  SizeBufT { sizeBufAllowOverflow :: Bool
           , sizeBufOverflowed    :: Bool
           , sizeBufData          :: UV.Vector Int8
           , sizeBufMaxSize       :: Int
           , sizeBufCurSize       :: Int
           , sizeBufReadCount     :: Int
           }
