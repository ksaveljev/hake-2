{-# LANGUAGE TemplateHaskell #-}
module Client.VIDGlobals ( module Client.VIDGlobals
                         , module Client.VidModeT
                         ) where

import Control.Lens (makeLenses)
import qualified Data.Vector as V

import Internal
import Client.VidModeT
import qualified Client.VidModes as VidModes

makeLenses ''VIDGlobals

initialVIDGlobals :: VIDGlobals
initialVIDGlobals =
  VIDGlobals { _vgVidModes      = VidModes.vidModes
             , _vgRefLibActive  = False
             , _vgFSModes       = Nothing
             , _vgFSResolutions = V.empty
             , _vgRefs          = V.empty
             , _vgDrivers       = V.empty
             }
