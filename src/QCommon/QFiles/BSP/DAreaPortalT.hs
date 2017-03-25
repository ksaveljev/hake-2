{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DAreaPortalT
    ( module QCommon.QFiles.BSP.DAreaPortalT
    ) where

import           Control.Lens    (makeLenses)
import           Data.Binary.Get (Get)

import           Types
import           Util.Binary     (getInt)

makeLenses ''DAreaPortalT

dAreaPortalTSize :: Int
dAreaPortalTSize = 8

getDAreaPortalT :: Get DAreaPortalT
getDAreaPortalT = DAreaPortalT <$> getInt <*> getInt

emptyDAreaPortalT :: DAreaPortalT
emptyDAreaPortalT = DAreaPortalT 0 0