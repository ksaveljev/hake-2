{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.EntInteractAdapter where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Quake
import QuakeState

data EntInteractAdapter =
  EntInteractAdapter { _getId    :: Quake B.ByteString
                     , _interact :: QuakeLens EdictT -> QuakeLens EdictT -> Quake ()
                     }

makeLenses ''EntInteractAdapter
