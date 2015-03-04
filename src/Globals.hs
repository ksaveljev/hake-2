{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Globals ( Globals
               , defaultGlobals
               , curtime
               , dedicated
               , nostdout
               , cmdText
               , module Game.CVarT
               ) where

import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Game.CVarT
import QCommon.SizeBufT

data Globals =
  Globals { _curtime   :: Int
          , _dedicated :: CVarT
          , _nostdout  :: CVarT

          , _cmdText   :: SizeBufT
          }

makeLenses ''Globals

defaultGlobals :: Globals
defaultGlobals =
  Globals { _curtime   = 0
          , _dedicated = newCVarT
          , _nostdout  = newCVarT
          
          , _cmdText   = SizeBufT False False UV.empty 0 0 0
          }
