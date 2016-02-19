{-# LANGUAGE TemplateHaskell #-}
module QCommon.SizeBufT
  ( module QCommon.SizeBufT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''SizeBufT

newSizeBufT :: SizeBufT
newSizeBufT = SizeBufT False False "" 0 0 0