{-# LANGUAGE TemplateHaskell #-}
module QCommon.PackFileT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

packFileSize :: Int
packFileSize = 64

packFileNameSize :: Int
packFileNameSize = 56

data PackFileT =
  PackFileT { _pfName    :: B.ByteString
            , _pfFilePos :: Int
            , _pfFileLen :: Int
            }

makeLenses ''PackFileT
