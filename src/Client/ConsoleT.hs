{-# LANGUAGE TemplateHaskell #-}
module Client.ConsoleT where

import Control.Lens (makeLenses)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable.Mutable as MSV

import qualified Constants

data ConsoleT =
  ConsoleT { _cInitialized :: Bool
           , _cText        :: MSV.IOVector Char
           , _cCurrent     :: Int
           , _cX           :: Int
           , _cDisplay     :: Int
           , _cOrMask      :: Int
           , _cLineWidth   :: Int
           , _cTotalLines  :: Int
           , _cCursorSpeed :: Float
           , _cVisLines    :: Int
           , _cTimes       :: UV.Vector Float
           }

makeLenses ''ConsoleT

newConsoleT :: ConsoleT
newConsoleT =
  ConsoleT { _cInitialized = False
           , _cText        = unsafePerformIO $ MSV.replicate Constants.conTextSize ' '
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
