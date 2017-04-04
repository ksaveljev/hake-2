{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DAreaT where

import           Control.Applicative  ((<*>))
import           Control.Lens         (makeLenses)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor         ((<$>))

import           Types
import           Util.Binary

dAreaTSize :: Int
dAreaTSize = 8

makeLenses ''DAreaT

newDAreaT :: BL.ByteString -> DAreaT
newDAreaT = runGet getDAreaT
  where getDAreaT :: Get DAreaT
        getDAreaT = DAreaT <$> getInt <*> getInt