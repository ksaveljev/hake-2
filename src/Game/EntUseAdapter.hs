{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.EntUseAdapter where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Quake
import QuakeState

data EntUseAdapter =
  EntUseAdapter { _getId :: Quake B.ByteString
                , _use   :: QuakeLens EdictT -> QuakeLens EdictT -> QuakeLens EdictT -> Quake ()
                }

makeLenses ''EntUseAdapter
