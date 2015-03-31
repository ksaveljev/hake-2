{-# LANGUAGE TemplateHaskell #-}
module Server.AreaNodeT ( AreaNodeT(..)
                        , module Server.AreaNodeT
                        ) where

import Control.Lens (makeLenses)

import Internal
import qualified Constants

makeLenses ''AreaNodeT

newAreaNodeT :: Int -> AreaNodeT
newAreaNodeT idx =
  AreaNodeT { _anAxis          = 0
            , _anDist          = 0
            , _anChildren      = (Nothing, Nothing)
            , _anTriggerEdicts = LinkReference idx
            , _anSolidEdicts   = LinkReference (Constants.areaNodes + idx)
            }
