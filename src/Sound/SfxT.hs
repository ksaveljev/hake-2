{-# LANGUAGE TemplateHaskell #-}
module Sound.SfxT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Sound.SfxCacheT

data SfxT =
  SfxT { _sfxName                 :: B.ByteString
       , _sfxRegistrationSequence :: Int
       , _sfxCache                :: SfxCacheT
       , _sfxTrueName             :: B.ByteString
       , _sfxBufferId             :: Int
       , _sfxIsCached             :: Bool
       }

makeLenses ''SfxT
