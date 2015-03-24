{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.AIAdapter where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Quake
import QuakeState

data AIAdapter =
  AIAdapter { _getId :: B.ByteString
            , _ai    :: QuakeLens EdictT -> Float -> Quake ()
            }

makeLenses ''AIAdapter
