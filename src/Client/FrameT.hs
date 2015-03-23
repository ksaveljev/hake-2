{-# LANGUAGE TemplateHaskell #-}
module Client.FrameT where

import Data.Word (Word8)
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Game.PlayerStateT

data FrameT =
  FrameT { _fValid         :: Bool
         , _fServerFrame   :: Int
         , _fDeltaFrame    :: Int
         , _fAreaBits      :: UV.Vector Word8
         , _fPlayerState   :: PlayerStateT
         , _fNumEntities   :: Int
         , _fParseEntities :: Int
         }

makeLenses ''FrameT

newFrameT :: FrameT
newFrameT = undefined -- TODO
