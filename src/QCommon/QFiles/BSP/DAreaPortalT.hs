{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DAreaPortalT
  ( module QCommon.QFiles.BSP.DAreaPortalT
  ) where

import Types

import Control.Lens (makeLenses)

makeLenses ''DAreaPortalT

emptyDAreaPortalT :: DAreaPortalT
emptyDAreaPortalT = DAreaPortalT 0 0