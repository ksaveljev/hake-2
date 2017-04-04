{-# LANGUAGE TemplateHaskell #-}
module Client.SCRGlobals where

import           Control.Lens       (makeLenses)

import           Client.CinematicsT
import           Client.DirtyT
import           Types

makeLenses ''SCRGlobals

initialSCRGlobals :: SCRGlobals
initialSCRGlobals = SCRGlobals
    { _scrConCurrent      = 0
    , _scrConLines        = 0
    , _scrInitialized     = False
    , _scrDrawLoading     = 0
    , _scrDirty           = newDirtyT
    , _scrOldDirty        = (newDirtyT, newDirtyT)
    , _scrCrosshairPic    = ""
    , _scrCrosshairWidth  = 0
    , _scrCrosshairHeight = 0
    , _scrLastFrames      = 0
    , _scrLastTime        = 0
    , _scrFPSValue        = ""
    , _scrCin             = newCinematicsT
    , _scrCenterString    = ""
    , _scrCenterTimeStart = 0
    , _scrCenterTimeOff   = 0
    , _scrCenterLines     = 0
    }