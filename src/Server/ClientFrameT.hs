{-# LANGUAGE TemplateHaskell #-}
module Server.ClientFrameT where

import Data.Word (Word8)
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Game.PlayerStateT

data ClientFrameT =
  ClientFrameT { _cfAreaBytes   :: Int
               , _cfAreaBits    :: UV.Vector Word8
               , _cfPlayerState :: PlayerStateT
               , _cfNumEntities :: Int
               , _cfFirstEntity :: Int
               , _cfSentTime    :: Int
               }

makeLenses ''ClientFrameT
