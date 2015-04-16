{-# LANGUAGE TemplateHaskell #-}
module Client.VIDGlobals ( module Client.VIDGlobals
                         , module Client.VidModeT
                         ) where

import Control.Lens (makeLenses)

import Internal
import Client.VidModeT
import qualified Client.VidModes as VidModes

makeLenses ''VIDGlobals

initialVIDGlobals :: VIDGlobals
initialVIDGlobals =
  VIDGlobals { _vgVidModes = VidModes.vidModes
             }
