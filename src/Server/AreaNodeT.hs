{-# LANGUAGE TemplateHaskell #-}
module Server.AreaNodeT where

import Control.Lens (makeLenses)

import Game.LinkT

data AreaNodeT =
  AreaNodeT { _anAxis          :: Int
            , _anDist          :: Float
            , _anChildren      :: (AreaNodeT, AreaNodeT)
            , _anTriggerEdicts :: LinkT
            , _anSolidEdicts   :: LinkT
            }

makeLenses ''AreaNodeT