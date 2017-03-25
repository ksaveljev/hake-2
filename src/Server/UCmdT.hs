{-# LANGUAGE TemplateHaskell #-}
module Server.UCmdT
    ( module Server.UCmdT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''UCmdT