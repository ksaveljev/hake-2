{-# LANGUAGE TemplateHaskell #-}
module Server.ClientFrameT where

import           Control.Lens         (makeLenses)
import qualified Data.Vector.Storable as SV
import           Data.Word            (Word8)

import qualified Constants
import           Game.PlayerStateT
import           Types

makeLenses ''ClientFrameT

newClientFrameT :: ClientFrameT
newClientFrameT = ClientFrameT
    { _cfAreaBytes   = 0
    , _cfAreaBits    = SV.replicate (Constants.maxMapAreas `div` 8) 0
    , _cfPlayerState = newPlayerStateT
    , _cfNumEntities = 0
    , _cfFirstEntity = 0
    , _cfSentTime    = 0
    }