{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DBrushT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Functor ((<$>))
import qualified Data.ByteString.Lazy as BL

dBrushTSize :: Int
dBrushTSize = 4 + 4 + 4

data DBrushT =
  DBrushT { _dbFirstSide :: Int
          , _dbNumSides  :: Int
          , _dbContents  :: Int
          }

makeLenses ''DBrushT

newDBrushT :: BL.ByteString -> DBrushT
newDBrushT = runGet getDBrushT
  where getDBrushT :: Get DBrushT
        getDBrushT = DBrushT <$> getInt
                             <*> getInt
                             <*> getInt

        getInt :: Get Int
        getInt = fromIntegral <$> getWord32le
