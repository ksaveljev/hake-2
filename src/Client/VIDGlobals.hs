{-# LANGUAGE TemplateHaskell #-}
module Client.VIDGlobals
    ( module Client.VIDGlobals
    ) where

import           Control.Lens    (makeLenses)
import qualified Data.Vector     as V

import qualified Client.VidModes as VidModes
import           Types

makeLenses ''VIDGlobals

initialVIDGlobals :: VIDGlobals
initialVIDGlobals = VIDGlobals
    { _vgVidModes           = VidModes.vidModes
    , _vgRefLibActive       = False
    , _vgFSModes            = Nothing
    , _vgFSResolutions      = V.empty
    , _vgModeX              = 0
    , _vgRefs               = V.empty
    , _vgDrivers            = V.empty
    , _vgCurrentMenu        = Nothing
    }