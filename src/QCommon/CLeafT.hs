{-# LANGUAGE TemplateHaskell #-}
module QCommon.CLeafT where

import Control.Lens (makeLenses)
import Data.Int (Int16)

data CLeafT =
  CLeafT { _clContents       :: Int
         , _clCluster        :: Int
         , _clArea           :: Int
         , _clFirstLeafBrush :: Int16
         , _clNumLeafBrushes :: Int16
         }

makeLenses ''CLeafT

newCLeafT :: CLeafT
newCLeafT = CLeafT 0 0 0 0 0
