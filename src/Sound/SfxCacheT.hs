{-# LANGUAGE TemplateHaskell #-}
module Sound.SfxCacheT
  ( module Sound.SfxCacheT
  ) where

import Types

import Control.Lens (makeLenses)

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
