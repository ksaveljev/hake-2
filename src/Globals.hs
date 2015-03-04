{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Globals ( Globals
               , defaultGlobals
               , curtime
               , dedicated
               , nostdout
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
          , _dedicated = CVarT "" "" "" 0 True 0.0 Nothing -- doesn't matter as it gets set in Main
          , _nostdout  = CVarT "" "" "" 0 True 0.0 Nothing -- doesn't matter as it gets set in Main
          
          , _cmdText   = SizeBufT False False UV.empty 0 0 0
          }
