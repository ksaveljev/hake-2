{-# LANGUAGE TemplateHaskell #-}
module Game.PlayerStateT
  ( module Game.PlayerStateT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''PlayerStateT