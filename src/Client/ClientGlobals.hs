{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.ClientGlobals ( module Client.ClientGlobals
                            , module Client.KButtonT
                            , module QCommon.SizeBufT
                            ) where

import Control.Lens (makeLenses)
import Client.KButtonT
import QCommon.SizeBufT

import Internal

makeLenses ''ClientGlobals

initialClientGlobals :: ClientGlobals
initialClientGlobals =
  ClientGlobals { _cgExtraTime       = 0
                , _cgNumCheatVars    = 0
                , _cgBuf             = newSizeBufT
                , _cgFrameMsec       = 0
                , _cgOldSysFrameTime = 0
                , _cgInKLook         = newKButtonT
                , _cgInLeft          = newKButtonT
                , _cgInRight         = newKButtonT
                , _cgInForward       = newKButtonT
                , _cgInBack          = newKButtonT
                , _cgInLookUp        = newKButtonT
                , _cgInLookDown      = newKButtonT
                , _cgInMoveLeft      = newKButtonT
                , _cgInMoveRight     = newKButtonT
                , _cgInStrafe        = newKButtonT
                , _cgInSpeed         = newKButtonT
                , _cgInUse           = newKButtonT
                , _cgInAttack        = newKButtonT
                , _cgInUp            = newKButtonT
                , _cgInDown          = newKButtonT
                , _cgInImpulse       = 0
                }
