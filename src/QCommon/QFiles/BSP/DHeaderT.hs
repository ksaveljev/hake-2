{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module QCommon.QFiles.BSP.DHeaderT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Functor ((<$>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import QCommon.LumpT
import Util.Binary
import qualified Constants

idBSPHeader :: B.ByteString
idBSPHeader = "IBSP"

data DHeaderT =
  DHeaderT { _dhIdent   :: Int
           , _dhVersion :: Int
           , _dhLumps   :: V.Vector LumpT
           }

makeLenses ''DHeaderT

newDHeaderT :: BL.ByteString -> DHeaderT
newDHeaderT = runGet getDHeaderT
  where getDHeaderT :: Get DHeaderT
        getDHeaderT = DHeaderT <$> getInt
                               <*> getInt
                               <*> getLumps

        getLumps :: Get (V.Vector LumpT)
        getLumps = V.sequence $ V.replicate Constants.headerLumps getLumpT

        getLumpT :: Get LumpT
        getLumpT = LumpT <$> getInt <*> getInt
