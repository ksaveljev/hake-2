{-# LANGUAGE TemplateHaskell #-}
module QCommon.PackT where

import Control.Lens (makeLenses)
import System.IO (Handle)
import qualified Data.ByteString as B
import qualified Data.Map as M

import QCommon.PackFileT

data PackT =
  PackT { _pFilename   :: B.ByteString
        , _pHandle     :: Handle
        , _pBackBuffer :: B.ByteString
        , _pNumFiles   :: Int
        , _pFiles      :: M.Map B.ByteString PackFileT
        }

makeLenses ''PackT
