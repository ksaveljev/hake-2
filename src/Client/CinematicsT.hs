{-# LANGUAGE TemplateHaskell #-}
module Client.CinematicsT
  ( module Client.CinematicsT
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector.Unboxed as UV

makeLenses ''CinematicsT

newCinematicsT :: CinematicsT
newCinematicsT =
  CinematicsT { _cRestartSound = False
              , _cSRate        = 0
              , _cSWidth       = 0
              , _cSChannels    = 0
              , _cWidth        = 0
              , _cHeight       = 0
              , _cPic          = Nothing
              , _cPicPending   = Nothing
              , _cNumHNodes1   = UV.replicate 256 0
              , _cHUsed        = UV.replicate 512 0
              , _cHCount       = UV.replicate 512 0
              }