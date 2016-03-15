{-# LANGUAGE TemplateHaskell #-}
module QCommon.SizeBufT
  ( module QCommon.SizeBufT
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B

makeLenses ''SizeBufT

newSizeBufT :: SizeBufT
newSizeBufT = SizeBufT False False B.empty 0 0 0