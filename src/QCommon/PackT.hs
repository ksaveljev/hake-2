{-# LANGUAGE TemplateHaskell #-}
module QCommon.PackT
    ( module QCommon.PackT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''PackT