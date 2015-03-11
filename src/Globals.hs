{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Globals ( module Globals
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
  Globals { _curtime            = 0
          , _cmdWait            = False

          , _aliasCount         = 0
          , _cTraces            = 0
          , _cBrushTraces       = 0
          , _cPointContents     = 0
          , _serverState        = 0
          
          , _netMessage         = newSizeBufT
          , _netMessageBuffer   = ""
          , _cmdText            = newSizeBufT
          , _cmdTextBuf         = ""

          , _cvarVars           = Seq.empty

          , _keyBindings        = V.replicate 256 Nothing
          , _keyDown            = UV.empty
          , _chatTeam           = False
          , _chatBuffer         = ""
          , _keyLines           = V.empty
          , _keyLinePos         = 0
          , _editLine           = 0
          }
