{-# LANGUAGE TemplateHaskell #-}
module QCommon.PackFileT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Types

packFileSize :: Int
packFileSize = 64

packFileNameSize :: Int
packFileNameSize = 56

makeLenses ''PackFileT
