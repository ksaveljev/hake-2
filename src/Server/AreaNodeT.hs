{-# LANGUAGE TemplateHaskell #-}
module Server.AreaNodeT
    ( module Server.AreaNodeT
    ) where

import           Control.Lens (makeLenses)

import qualified Constants
import           Types

makeLenses ''AreaNodeT

newAreaNodeT :: Int -> AreaNodeT
newAreaNodeT idx = AreaNodeT
    { _anAxis          = 0
    , _anDist          = 0
    , _anChildren      = (Nothing, Nothing)
    , _anTriggerEdicts = Ref idx
    , _anSolidEdicts   = Ref (Constants.areaNodes + idx)
    }