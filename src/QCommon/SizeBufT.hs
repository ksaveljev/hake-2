{-# LANGUAGE TemplateHaskell #-}
module QCommon.SizeBufT where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B

import           Types

makeLenses ''SizeBufT

newSizeBufT :: SizeBufT
newSizeBufT = SizeBufT False False "" 0 0 0