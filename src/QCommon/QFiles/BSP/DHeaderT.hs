{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DHeaderT where

import           Control.Applicative  ((<*>))
import           Control.Lens         (makeLenses)
import           Data.Functor         ((<$>))
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector          as V

import qualified Constants
import           QCommon.LumpT
import           Types
import           Util.Binary

idBSPHeader :: B.ByteString
idBSPHeader = "IBSP"

makeLenses ''DHeaderT

newDHeaderT :: BL.ByteString -> DHeaderT
newDHeaderT = runGet getDHeaderT
  where getDHeaderT :: Get DHeaderT
        getDHeaderT = DHeaderT <$> getInt
                               <*> getInt
                               <*> getLumps

        getLumps :: Get (V.Vector LumpT)
        getLumps = V.replicateM Constants.headerLumps getLumpT

        getLumpT :: Get LumpT
        getLumpT = LumpT <$> getInt <*> getInt

getDHeaderT :: Get DHeaderT
getDHeaderT =
    DHeaderT <$> getInt <*> getInt <*> getLumps
  where
    getLumps = V.replicateM Constants.headerLumps getLumpT