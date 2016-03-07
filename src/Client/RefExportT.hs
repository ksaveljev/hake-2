{-# LANGUAGE TemplateHaskell #-}
module Client.RefExportT
  ( module Client.RefExportT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''RefExportT