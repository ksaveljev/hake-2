{-# LANGUAGE TemplateHaskell #-}
module QCommon.CLeafT where

import           Control.Lens (makeLenses)
import           Data.Word    (Word16)

import           Types

makeLenses ''CLeafT

newCLeafT :: CLeafT
newCLeafT = CLeafT 0 0 0 0 0
