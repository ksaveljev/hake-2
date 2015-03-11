{-# LANGUAGE TemplateHaskell #-}
module Client.CLSustainT where

import Linear.V3 (V3)
import Control.Lens (makeLenses)

import Quake

data CLSustainT =
  CLSustainT { _clsId            :: Int
             , _clsType          :: Int
             , _clsEndTime       :: Int
             , _clsNextThink     :: Int
             , _clsThinkInterval :: Int
             , _clsOrg           :: V3 Float
             , _clsDir           :: V3 Float
             , _clsColor         :: Int
             , _clsCount         :: Int
             , _clsMagnitude     :: Int
             , _clsThink         :: Quake () -- TODO
             }

makeLenses ''CLSustainT
