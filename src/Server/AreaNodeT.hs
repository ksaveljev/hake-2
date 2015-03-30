{-# LANGUAGE TemplateHaskell #-}
module Server.AreaNodeT where

import Control.Lens (makeLenses)

import qualified Constants

data AreaNodeT =
  AreaNodeT { _anAxis          :: Int
            , _anDist          :: Float
            , _anChildren      :: (Int, Int) -- indexes to svGlobals.svAreaNodes
            , _anTriggerEdicts :: Int -- index to svGlobals.svLinks
            , _anSolidEdicts   :: Int -- index to svGlobals.svLinks
            }

makeLenses ''AreaNodeT

newAreaNodeT :: Int -> AreaNodeT
newAreaNodeT idx =
  AreaNodeT { _anAxis          = 0
            , _anDist          = 0
            , _anChildren      = (0, 0) -- indexes to svGlobals.svAreaNodes
            , _anTriggerEdicts = idx
            , _anSolidEdicts   = Constants.areaNodes + idx
            }
