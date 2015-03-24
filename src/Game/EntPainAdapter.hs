{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Game.EntPainAdapter where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B

import Quake
import QuakeState

data EntPainAdapter =
  EntPainAdapter { _getId :: Quake B.ByteString
                 , _pain  :: QuakeLens EdictT -> QuakeLens EdictT -> Float -> Int -> Quake ()
                 }

makeLenses ''EntPainAdapter
