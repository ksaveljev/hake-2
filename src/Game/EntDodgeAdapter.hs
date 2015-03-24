{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.EntDodgeAdapter where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Quake
import QuakeState

data EntDodgeAdapter =
  EntDodgeAdapter { _getId :: Quake B.ByteString
                  , _dodge :: QuakeLens EdictT -> QuakeLens EdictT -> Float -> Quake ()
                  }

makeLenses ''EntDodgeAdapter
