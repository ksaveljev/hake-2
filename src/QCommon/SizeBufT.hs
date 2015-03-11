{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module QCommon.SizeBufT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data SizeBufT =
  SizeBufT { _sbAllowOverflow :: Bool
           , _sbOverflowed    :: Bool
           , _sbData          :: B.ByteString
           , _sbMaxSize       :: Int
           , _sbCurSize       :: Int
           , _sbReadCount     :: Int
           }

makeLenses ''SizeBufT

newSizeBufT :: SizeBufT
newSizeBufT = SizeBufT False False "" 0 0 0
