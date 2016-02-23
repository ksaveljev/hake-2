{-# LANGUAGE TemplateHaskell #-}
module QuakeIOState where

import qualified Constants
import           Game.CPlaneT (newCPlaneT)
import           Game.EdictT (newEdictT)
import           QCommon.CBrushT (newCBrushT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as MSV
import           System.IO.Unsafe (unsafePerformIO)

makeLenses ''QuakeIOState

initialQuakeIOState :: QuakeIOState
initialQuakeIOState =
  QuakeIOState { _ioGEdicts    = unsafePerformIO (V.thaw (V.generate (Constants.maxEdicts + 1) newEdictT)) -- one extra for "dummy edict"
               , _ioMapPlanes  = unsafePerformIO (V.thaw (V.replicate (Constants.maxMapPlanes + 6) newCPlaneT))
               , _ioMapBrushes = unsafePerformIO (V.thaw (V.replicate Constants.maxMapBrushes newCBrushT))
               , _ioText       = unsafePerformIO (MSV.replicate Constants.conTextSize ' ')
               }
