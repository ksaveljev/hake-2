{-# LANGUAGE TemplateHaskell #-}
module Client.CLSustainT
    ( module Client.CLSustainT
    ) where

import           Control.Lens (makeLenses)
import           Linear (V3(..))

import           Types

makeLenses ''CLSustainT

newCLSustainT :: CLSustainT
newCLSustainT = CLSustainT
    { _clsId            = 0
    , _clsType          = 0
    , _clsEndTime       = 0
    , _clsNextThink     = 0
    , _clsThinkInterval = 0
    , _clsOrg           = V3 0 0 0
    , _clsDir           = V3 0 0 0
    , _clsColor         = 0
    , _clsCount         = 0
    , _clsMagnitude     = 0
    , _clsThink         = Nothing
    }