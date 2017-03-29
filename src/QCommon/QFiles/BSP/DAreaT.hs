{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DAreaT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import qualified Data.ByteString.Lazy as BL

import Types
import Util.Binary

dAreaTSize :: Int
dAreaTSize = 8

data DAreaT =
  DAreaT { _daNumAreaPortals  :: Int
         , _daFirstAreaPortal :: Int
         }

makeLenses ''DAreaT

newDAreaT :: BL.ByteString -> DAreaT
newDAreaT = runGet getDAreaT
  where getDAreaT :: Get DAreaT
        getDAreaT = DAreaT <$> getInt <*> getInt
