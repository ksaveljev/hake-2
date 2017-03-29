{-# LANGUAGE TemplateHaskell #-}
module Client.VIDGlobals ( module Client.VIDGlobals
                         , module Client.VidModeT
                         , module Client.MenuFrameworkS
                         , module Client.MenuItem
                         ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Types
import Client.MenuFrameworkS
import Client.MenuItem
import Client.VidModeT
import qualified Client.VidModes as VidModes

makeLenses ''VIDGlobals

initialVIDGlobals :: VIDGlobals
initialVIDGlobals =
  VIDGlobals { _vgVidModes           = VidModes.vidModes
             , _vgRefLibActive       = False
             , _vgFSModes            = Nothing
             , _vgFSResolutions      = V.empty
             , _vgModeX              = 0
             , _vgRefs               = V.empty
             , _vgDrivers            = V.empty
             , _vgCurrentMenu        = Nothing
             }
