{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DBrushSideT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Functor ((<$>))
import Data.Int (Int16)
import Data.Word (Word16)
import qualified Data.ByteString.Lazy as BL

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

        getInt16 :: Get Int16
        getInt16 = fromIntegral <$> getWord16le
