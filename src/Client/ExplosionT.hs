{-# LANGUAGE TemplateHaskell #-}
module Client.ExplosionT where

import Control.Lens (makeLenses)
import Linear (V3(..))

import Internal
import Client.EntityT

makeLenses ''ExplosionT

newExplosionT :: ExplosionT
newExplosionT =
  ExplosionT { _eType       = 0
             , _eEnt        = newEntityT
             , _eFrames     = 0
             , _eLight      = 0
             , _eLightColor = V3 0 0 0
             , _eStart      = 0
             , _eBaseFrame  = 0
             }
