{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DBrushT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import qualified Data.ByteString.Lazy as BL

import Types
import Util.Binary

dBrushTSize :: Int
dBrushTSize = 4 + 4 + 4

makeLenses ''DBrushT

newDBrushT :: BL.ByteString -> DBrushT
newDBrushT = runGet getDBrushT
  where getDBrushT :: Get DBrushT
        getDBrushT = DBrushT <$> getInt
                             <*> getInt
                             <*> getInt
