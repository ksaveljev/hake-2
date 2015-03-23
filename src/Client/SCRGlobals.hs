{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.SCRGlobals ( module Client.SCRGlobals
                         , module Client.DirtyT
                         ) where

import Control.Lens (makeLenses)

import Internal
import Client.DirtyT

makeLenses ''SCRGlobals

initialSCRGlobals :: SCRGlobals
initialSCRGlobals =
  SCRGlobals { _scrConCurrent      = 0
             , _scrConLines        = 0
             , _scrInitialized     = False
             , _scrDrawLoading     = 0
             , _scrDirty           = newDirtyT
             , _scrOldDirty        = (newDirtyT, newDirtyT)
             , _scrCrosshairPic    = ""
             , _scrCrosshairWidth  = 0
             , _scrCrosshairHeight = 0
             }
