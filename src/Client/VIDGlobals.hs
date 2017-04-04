{-# LANGUAGE TemplateHaskell #-}
module Client.VIDGlobals where

import           Control.Lens    (makeLenses)
import qualified Data.Vector     as V

import           Client.VidModes (vidModes)
import           Types

makeLenses ''VIDGlobals

initialVIDGlobals :: VIDGlobals
initialVIDGlobals = VIDGlobals
    { _vgVidModes           = vidModes
    , _vgRefLibActive       = False
    , _vgFSModes            = Nothing
    , _vgFSResolutions      = V.empty
    , _vgModeX              = 0
    , _vgRefs               = V.empty
    , _vgDrivers            = V.empty
    , _vgCurrentMenu        = Nothing
    }