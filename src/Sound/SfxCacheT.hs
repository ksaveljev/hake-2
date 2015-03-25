{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
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

newSfxCacheT :: SfxCacheT
newSfxCacheT =
  SfxCacheT { _scLength    = 0
            , _scLoopStart = 0
            , _scSpeed     = 0
            , _scWidth     = 0
            , _scStereo    = 0
            , _scData      = ""
            }
