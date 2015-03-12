{-# LANGUAGE TemplateHaskell #-}
module Sound.SfxCacheT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

data SfxCacheT =
  SfxCacheT { _scLength    :: Int
            , _scLoopStart :: Int
            , _scSpeed     :: Int
            , _scWidth     :: Int
            , _scStereo    :: Int
            , _scData      :: B.ByteString
            }

makeLenses ''SfxCacheT
