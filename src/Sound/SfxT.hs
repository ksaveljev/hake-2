{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Sound.SfxT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Sound.SfxCacheT

data SfxT =
  SfxT { _sfxName                 :: B.ByteString
       , _sfxRegistrationSequence :: Int
       , _sfxCache                :: Maybe SfxCacheT
       , _sfxTrueName             :: B.ByteString
       , _sfxBufferId             :: Int
       , _sfxIsCached             :: Bool
       }

makeLenses ''SfxT

newSfxT :: SfxT
newSfxT =
  SfxT { _sfxName                 = ""
       , _sfxRegistrationSequence = 0
       , _sfxCache                = Nothing
       , _sfxTrueName             = ""
       , _sfxBufferId             = -1
       , _sfxIsCached             = False
       }
