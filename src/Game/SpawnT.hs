{-# LANGUAGE TemplateHaskell #-}
module Game.SpawnT ( SpawnT(..)
                   , module Game.SpawnT
                   ) where

import Control.Lens (makeLenses)

import Internal

makeLenses ''SpawnT
