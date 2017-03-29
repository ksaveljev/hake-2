{-# LANGUAGE TemplateHaskell #-}
module Game.SpawnT ( SpawnT(..)
                   , module Game.SpawnT
                   ) where

import Control.Lens (makeLenses)

import Types

makeLenses ''SpawnT
