{-# LANGUAGE TemplateHaskell #-}
module Game.ClientPersistantT where

import Control.Lens (makeLenses)

import Internal

makeLenses ''ClientPersistantT
