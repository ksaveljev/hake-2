{-# LANGUAGE TemplateHaskell #-}
module Sound.SfxT where

import           Control.Lens    (makeLenses)
import qualified Data.ByteString as B

import           Types

makeLenses ''SfxT

newSfxT :: SfxT
newSfxT = SfxT
    { _sfxName                 = ""
    , _sfxRegistrationSequence = 0
    , _sfxCache                = Nothing
    , _sfxTrueName             = ""
    , _sfxBufferId             = -1
    , _sfxIsCached             = False
    }