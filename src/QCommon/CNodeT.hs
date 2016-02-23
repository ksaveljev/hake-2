{-# LANGUAGE TemplateHaskell #-}
module QCommon.CNodeT
  ( module QCommon.CNodeT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''CNodeT

newCNodeT :: CNodeT
newCNodeT = CNodeT Nothing (0, 0)