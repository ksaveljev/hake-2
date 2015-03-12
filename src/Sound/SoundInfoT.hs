{-# LANGUAGE TemplateHaskell #-}
module Sound.SoundInfoT where

import Control.Lens (makeLenses)

data SoundInfoT =
  SoundInfoT { _siChannels        :: Int
             , _siSamples         :: Int
             , _siSubmissionChunk :: Int
             , _siSamplePos       :: Int
             , _siSampleBits      :: Int
             , _siSpeed           :: Int
             }

makeLenses ''SoundInfoT
