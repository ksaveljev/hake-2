{-# LANGUAGE TemplateHaskell #-}
module Game.GClientT
  ( module Game.GClientT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''GClientT