{-# LANGUAGE TemplateHaskell #-}
module QCommon.CLeafT
    ( module QCommon.CLeafT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''CLeafT

newCLeafT :: CLeafT
newCLeafT = CLeafT 0 0 0 0 0