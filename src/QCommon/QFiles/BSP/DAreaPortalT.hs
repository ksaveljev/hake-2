{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DAreaPortalT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import qualified Data.ByteString.Lazy as BL

import Util.Binary

dAreaPortalTSize :: Int
dAreaPortalTSize = 8

data DAreaPortalT =
  DAreaPortalT { _dapPortalNum :: Int
               , _dapOtherArea :: Int
               }

makeLenses ''DAreaPortalT

emptyDAreaPortalT :: DAreaPortalT
emptyDAreaPortalT = DAreaPortalT 0 0

newDAreaPortalT :: BL.ByteString -> DAreaPortalT
newDAreaPortalT = runGet getDAreaPortalT
  where getDAreaPortalT :: Get DAreaPortalT
        getDAreaPortalT = DAreaPortalT <$> getInt <*> getInt
