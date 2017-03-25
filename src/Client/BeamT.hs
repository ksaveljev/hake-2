{-# LANGUAGE TemplateHaskell #-}
module Client.BeamT
    ( module Client.BeamT
    ) where

import           Control.Lens (makeLenses)
import           Linear       (V3(..))

import           Types

makeLenses ''BeamT

newBeamT :: BeamT
newBeamT = BeamT
    { _bEntity     = 0
    , _bDestEntity = 0
    , _bModel      = Nothing
    , _bEndTime    = 0
    , _bOffset     = V3 0 0 0
    , _bStart      = V3 0 0 0
    , _bEnd        = V3 0 0 0
    }