{-# LANGUAGE TemplateHaskell #-}
module QCommon.CLeafT where

import Control.Lens (makeLenses)
import Data.Word (Word16)

data CLeafT =
  CLeafT { _clContents       :: Int
         , _clCluster        :: Int
         , _clArea           :: Int
         , _clFirstLeafBrush :: Word16
         , _clNumLeafBrushes :: Word16
         }

makeLenses ''CLeafT

newCLeafT :: CLeafT
newCLeafT = CLeafT 0 0 0 0 0
