{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DBrushSideT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import Data.Int (Int16)
import Data.Word (Word16)
import qualified Data.ByteString.Lazy as BL

import Types
import Util.Binary

dBrushSideTSize :: Int
dBrushSideTSize = 4

data DBrushSideT =
  DBrushSideT { _dbsPlaneNum :: Word16
              , _dbsTexInfo  :: Int16
              }

makeLenses ''DBrushSideT

newDBrushSideT :: BL.ByteString -> DBrushSideT
newDBrushSideT = runGet getDBrushSideT
  where getDBrushSideT :: Get DBrushSideT
        getDBrushSideT = DBrushSideT <$> getWord16le
                                     <*> getInt16
