{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Globals ( Globals
               , initialGlobals
               , curtime
               , cmdWait
               , aliasCount
               , dedicated
               , nostdout
               , cmdText
               , cmdTextBuf
               , cvarVars
               , keyBindings
               , keyDown
               , chatTeam
               , chatBuffer
               , keyLines
               , keyLinePos
               , editLine
               , module Game.CVarT
               , module QCommon.SizeBufT
               ) where

import Control.Lens (makeLenses)
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Game.CVarT
import QCommon.SizeBufT

import Internal

makeLenses ''Globals

initialGlobals :: Globals
initialGlobals =
  Globals { _curtime     = 0
          , _cmdWait     = False

          , _aliasCount  = 0

          , _dedicated   = newCVarT
          , _nostdout    = newCVarT
          
          , _cmdText     = SizeBufT False False "" 0 0 0
          , _cmdTextBuf  = ""

          , _cvarVars    = Seq.empty

          , _keyBindings = V.replicate 256 Nothing
          , _keyDown     = UV.empty
          , _chatTeam    = False
          , _chatBuffer  = ""
          , _keyLines    = V.empty
          , _keyLinePos  = 0
          , _editLine    = 0
          }
