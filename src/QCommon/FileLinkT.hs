{-# LANGUAGE TemplateHaskell #-}
module QCommon.FileLinkT
  (module QCommon.FileLinkT)
  where

import Types

import Control.Lens (makeLenses)

makeLenses ''FileLinkT