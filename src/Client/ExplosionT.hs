{-# LANGUAGE TemplateHaskell #-}
module Client.ExplosionT
    ( module Client.ExplosionT
    ) where

import           Control.Lens     (makeLenses)
import           Data.IORef       (newIORef)
import           Linear           (V3(..))
import           System.IO.Unsafe (unsafePerformIO)

import           Client.EntityT   (newEntityT)
import           Types

makeLenses ''ExplosionT

newExplosionT :: ExplosionT
newExplosionT = ExplosionT
    { _eType       = 0
    , _eEnt        = unsafePerformIO (newIORef newEntityT)
    , _eFrames     = 0
    , _eLight      = 0
    , _eLightColor = V3 0 0 0
    , _eStart      = 0
    , _eBaseFrame  = 0
    }