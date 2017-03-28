{-# LANGUAGE TemplateHaskell #-}
module Client.FrameT where

import Data.Word (Word8)
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Game.PlayerStateT

data FrameT =
  FrameT { _fValid         :: Bool
         , _fServerFrame   :: Int
         , _fServerTime    :: Int
         , _fDeltaFrame    :: Int
         , _fAreaBits      :: UV.Vector Word8
         , _fPlayerState   :: PlayerStateT
         , _fNumEntities   :: Int
         , _fParseEntities :: Int
         }

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
