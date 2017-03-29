{-# LANGUAGE TemplateHaskell #-}
module Sound.WavInfoT where

import Control.Lens (makeLenses)

import Types

data WavInfoT =
  WavInfoT { _wiRate      :: Int
           , _wiWidth     :: Int
           , _wiChannels  :: Int
           , _wiLoopStart :: Int
           , _wiSamples   :: Int
           , _wiDataOfs   :: Int
           }

makeLenses ''WavInfoT
