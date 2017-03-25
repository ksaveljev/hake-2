{-# LANGUAGE TemplateHaskell #-}
module Client.RefExportT
    ( module Client.RefExportT
    ) where

import           Control.Lens (makeLenses)

import           Types

makeLenses ''RefExportT