{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Globals ( Globals
               , initialGlobals
               , curtime
               , cmdWait
               , dedicated
               , nostdout
               , cmdText
               , cmdTextBuf
               , cvarVars
               , module Game.CVarT
               ) where

import Control.Lens (makeLenses)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as UV

import Game.CVarT
import QCommon.SizeBufT

import Internal

makeLenses ''Globals

initialGlobals :: Globals
initialGlobals =
  Globals { _curtime    = 0
          , _cmdWait    = False
          , _dedicated  = newCVarT
          , _nostdout   = newCVarT
          
          , _cmdText    = SizeBufT False False UV.empty 0 0 0
          , _cmdTextBuf = UV.replicate 8192 0

          , _cvarVars   = Seq.empty
          }
