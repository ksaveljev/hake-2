{-# LANGUAGE TemplateHaskell #-}
module Client.ConsoleT
  ( module Client.ConsoleT
  ) where

import qualified Constants
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

makeLenses ''ConsoleT

newConsoleT :: ConsoleT
newConsoleT =
  ConsoleT { _cInitialized = False
           , _cCurrent     = 0
           , _cX           = 0
           , _cDisplay     = 0
           , _cOrMask      = 0
           , _cLineWidth   = 0
           , _cTotalLines  = 0
           , _cCursorSpeed = 0
           , _cVisLines    = 0
           , _cTimes       = UV.replicate Constants.numConTimes 0
           }