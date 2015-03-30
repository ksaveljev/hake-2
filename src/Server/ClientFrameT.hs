{-# LANGUAGE TemplateHaskell #-}
module Server.ClientFrameT where

import Data.Word (Word8)
import Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

import Game.PlayerStateT
import qualified Constants

data ClientFrameT =
  ClientFrameT { _cfAreaBytes   :: Int
               , _cfAreaBits    :: UV.Vector Word8
               , _cfPlayerState :: PlayerStateT
               , _cfNumEntities :: Int
               , _cfFirstEntity :: Int
               , _cfSentTime    :: Int
               }

makeLenses ''ClientFrameT

newClientFrameT :: ClientFrameT
newClientFrameT =
  ClientFrameT { _cfAreaBytes   = 0
               , _cfAreaBits    = UV.replicate (Constants.maxMapAreas `div` 8) 0
               , _cfPlayerState = newPlayerStateT
               , _cfNumEntities = 0
               , _cfFirstEntity = 0
               , _cfSentTime    = 0
               }
