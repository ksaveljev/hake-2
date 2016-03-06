{-# LANGUAGE TemplateHaskell #-}
module Sound.SfxT
  ( module Sound.SfxT
  ) where

import Types

import Control.Lens (makeLenses)

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
