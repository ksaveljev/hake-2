{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Globals ( Globals
               , defaultGlobals
               , curtime
               , cmdWait
               , dedicated
               , nostdout
               , cmdText
               , cmdTextBuf
               , module Game.CVarT
               ) where

import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Game.CVarT
import QCommon.SizeBufT

import Internal

makeLenses ''Globals

defaultGlobals :: Globals
defaultGlobals =
  Globals { _curtime    = 0
          , _cmdWait    = False
          , _dedicated  = newCVarT
          , _nostdout   = newCVarT
          
          , _cmdText    = SizeBufT False False UV.empty 0 0 0
          , _cmdTextBuf = UV.replicate 8192 0
          }
