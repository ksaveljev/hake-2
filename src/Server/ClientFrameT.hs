{-# LANGUAGE TemplateHaskell #-}
module Server.ClientFrameT
  ( module Server.ClientFrameT
  ) where

import qualified Constants
import           Game.PlayerStateT (newPlayerStateT)
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector.Storable as VS

makeLenses ''ClientFrameT

newClientFrameT :: ClientFrameT
newClientFrameT =
  ClientFrameT { _cfAreaBytes   = 0
               , _cfAreaBits    = VS.replicate (Constants.maxMapAreas `div` 8) 0
               , _cfPlayerState = newPlayerStateT
               , _cfNumEntities = 0
               , _cfFirstEntity = 0
               , _cfSentTime    = 0
               }