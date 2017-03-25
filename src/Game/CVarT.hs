{-# LANGUAGE TemplateHaskell #-}
module Game.CVarT
    ( module Game.CVarT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''CVarT