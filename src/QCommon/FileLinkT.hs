{-# LANGUAGE TemplateHaskell #-}
module QCommon.FileLinkT
    ( module QCommon.FileLinkT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''FileLinkT