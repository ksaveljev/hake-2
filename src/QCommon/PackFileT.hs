{-# LANGUAGE TemplateHaskell #-}
module QCommon.PackFileT
  (module QCommon.PackFileT)
  where

import Types

import Control.Lens (makeLenses)

makeLenses ''PackFileT