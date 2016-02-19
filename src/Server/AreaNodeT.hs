{-# LANGUAGE TemplateHaskell #-}
module Server.AreaNodeT
  ( module Server.AreaNodeT
  ) where

import qualified Constants
import           Types

import           Control.Lens (makeLenses)

makeLenses ''AreaNodeT

newAreaNodeT :: Int -> AreaNodeT
newAreaNodeT idx =
  AreaNodeT { _anAxis          = 0
            , _anDist          = 0
            , _anChildren      = (Nothing, Nothing)
            , _anTriggerEdicts = LinkRef idx
            , _anSolidEdicts   = LinkRef (Constants.areaNodes + idx)
            }