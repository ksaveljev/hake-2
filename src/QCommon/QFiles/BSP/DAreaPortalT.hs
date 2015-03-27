{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DAreaPortalT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Functor ((<$>))
import qualified Data.ByteString.Lazy as BL

data DAreaPortalT =
  DAreaPortalT { _dapPortalNum :: Int
               , _dapOtherArea :: Int
               }

makeLenses ''DAreaPortalT

newDAreaPortalT :: BL.ByteString -> DAreaPortalT
newDAreaPortalT = runGet getDAreaPortalT
  where getDAreaPortalT :: Get DAreaPortalT
        getDAreaPortalT = DAreaPortalT <$> getInt <*> getInt

        getInt :: Get Int
        getInt = fromIntegral <$> getWord32le