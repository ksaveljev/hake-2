{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Globals ( Globals
               , defaultGlobals
               , curtime
               , dedicated
               , nostdout
               , cmdText
               , cmdTextBuf
               , module Game.CVarT
               ) where

import Data.Word (Word8)
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Game.CVarT
import QCommon.SizeBufT

data Globals =
  Globals { _curtime    :: Int
          , _dedicated  :: CVarT
          , _nostdout   :: CVarT

          , _cmdText    :: SizeBufT
          , _cmdTextBuf :: UV.Vector Word8
          }

makeLenses ''Globals

defaultGlobals :: Globals
defaultGlobals =
  Globals { _curtime    = 0
          , _dedicated  = newCVarT
          , _nostdout   = newCVarT
          
          , _cmdText    = SizeBufT False False UV.empty 0 0 0
          , _cmdTextBuf = UV.replicate 8192 0
          }
