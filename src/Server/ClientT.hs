{-# LANGUAGE TemplateHaskell #-}
module Server.ClientT
  ( module Server.ClientT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''ClientT