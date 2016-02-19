{-# LANGUAGE TemplateHaskell #-}
module Game.ClientPersistantT
  ( module Game.ClientPersistantT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''ClientPersistantT