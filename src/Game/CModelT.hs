{-# LANGUAGE TemplateHaskell #-}
module Game.CModelT
  ( module Game.CModelT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''CModelT