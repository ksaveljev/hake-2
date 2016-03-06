{-# LANGUAGE TemplateHaskell #-}
module Client.FrameT
  ( module Client.FrameT
  ) where

import           Game.PlayerStateT (newPlayerStateT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

makeLenses ''FrameT

newFrameT :: FrameT
newFrameT =
  FrameT { _fValid         = False
         , _fServerFrame   = 0
         , _fServerTime    = 0
         , _fDeltaFrame    = 0
         , _fAreaBits      = UV.replicate 32 0 -- MAX_MAP_AREAS / 8 where MAX_MAP_AREAS = 256
         , _fPlayerState   = newPlayerStateT
         , _fNumEntities   = 0
         , _fParseEntities = 0
         }
