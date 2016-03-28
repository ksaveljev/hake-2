{-# LANGUAGE TemplateHaskell #-}
module Server.UCmdT
  ( module Server.UCmdT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''UCmdT