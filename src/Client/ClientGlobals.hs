{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.ClientGlobals where

import Control.Lens (makeLenses)
import QCommon.SizeBufT

import Internal

makeLenses ''ClientGlobals

initialClientGlobals :: ClientGlobals
initialClientGlobals =
  ClientGlobals { _cgExtraTime    = 0
                , _cgNumCheatVars = 0
                , _cgBuf          = newSizeBufT
                }
