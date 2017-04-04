{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DAreaPortalT where

import           Control.Applicative  ((<*>))
import           Control.Lens         (makeLenses)
import           Data.Binary.Get      (Get)
import           Data.Functor         ((<$>))
import qualified Data.ByteString.Lazy as BL

import           Types
import           Util.Binary

dAreaPortalTSize :: Int
dAreaPortalTSize = 8

makeLenses ''DAreaPortalT

emptyDAreaPortalT :: DAreaPortalT
emptyDAreaPortalT = DAreaPortalT 0 0

newDAreaPortalT :: BL.ByteString -> DAreaPortalT
newDAreaPortalT = runGet getDAreaPortalT
  where
    getDAreaPortalT :: Get DAreaPortalT
    getDAreaPortalT = DAreaPortalT <$> getInt <*> getInt

getDAreaPortalT :: Get DAreaPortalT
getDAreaPortalT = DAreaPortalT <$> getInt <*> getInt
