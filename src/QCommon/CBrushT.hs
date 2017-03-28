{-# LANGUAGE TemplateHaskell #-}
module QCommon.CBrushT where

import Control.Lens (makeLenses)

data CBrushT =
  CBrushT { _cbContents       :: Int
          , _cbNumSides       :: Int
          , _cbFirstBrushSide :: Int
          , _cbCheckCount     :: Int
          }

makeLenses ''CBrushT

newCBrushT :: CBrushT
newCBrushT = CBrushT 0 0 0 0
