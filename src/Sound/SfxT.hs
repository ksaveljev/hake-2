{-# LANGUAGE TemplateHaskell #-}
module Sound.SfxT
  ( module Sound.SfxT
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B

makeLenses ''SfxT

newSfxT :: SfxT
newSfxT =
  SfxT { _sfxName                 = B.empty
       , _sfxRegistrationSequence = 0
       , _sfxCache                = Nothing
       , _sfxTrueName             = B.empty
       , _sfxBufferId             = -1
       , _sfxIsCached             = False
       }
