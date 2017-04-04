{-# LANGUAGE TemplateHaskell #-}
module QCommon.CNodeT where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''CNodeT

newCNodeT :: CNodeT
newCNodeT = CNodeT Nothing (0, 0)