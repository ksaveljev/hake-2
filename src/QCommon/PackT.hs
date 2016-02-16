{-# LANGUAGE TemplateHaskell #-}
module QCommon.PackT
  (module QCommon.PackT)
  where

import Types

import Control.Lens (makeLenses)

makeLenses ''PackT