{-# LANGUAGE TemplateHaskell #-}
module Client.SCRGlobals
    ( module Client.SCRGlobals
    ) where

import           Control.Lens       (makeLenses)
import qualified Data.ByteString    as B

import           Client.CinematicsT (newCinematicsT)
import           Client.DirtyT      (newDirtyT)
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
    , _scrCrosshairPic    = B.empty
    , _scrCrosshairWidth  = 0
    , _scrCrosshairHeight = 0
    , _scrLastFrames      = 0
    , _scrLastTime        = 0
    , _scrFPSValue        = B.empty
    , _scrCin             = newCinematicsT
    , _scrCenterString    = B.empty
    , _scrCenterTimeStart = 0
    , _scrCenterTimeOff   = 0
    , _scrCenterLines     = 0
    }