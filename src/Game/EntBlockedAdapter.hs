{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.EntBlockedAdapter where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Quake
import QuakeState

data EntBlockedAdapter =
  EntBlockedAdapter { _getId   :: Quake B.ByteString
                    , _blocked :: QuakeLens EdictT -> QuakeLens EdictT -> Quake ()
                    }

makeLenses ''EntBlockedAdapter
