{-# LANGUAGE TemplateHaskell #-}
module QuakeIOState where

import qualified Constants
import           Game.EdictT (newEdictT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V
import           System.IO.Unsafe (unsafePerformIO)

makeLenses ''QuakeIOState

initialQuakeIOState :: QuakeIOState
initialQuakeIOState =
  QuakeIOState { _ioGEdicts = unsafePerformIO (V.thaw (V.generate (Constants.maxEdicts + 1) newEdictT)) -- one extra for "dummy edict"
               }
