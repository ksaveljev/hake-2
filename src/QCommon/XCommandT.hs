{-# LANGUAGE TemplateHaskell #-}
module QCommon.XCommandT
  ( module QCommon.XCommandT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''XCommandT