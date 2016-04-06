{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.BSP.DHeaderT
  ( module QCommon.QFiles.BSP.DHeaderT
  ) where

import qualified Constants
import           QCommon.LumpT (getLumpT)
import           Types
import           Util.Binary (getInt)

import           Control.Lens (makeLenses)
import           Data.Binary (Get)
import qualified Data.ByteString as B
import qualified Data.Vector as V

makeLenses ''DHeaderT

idBSPHeader :: B.ByteString
idBSPHeader = "IBSP"

getDHeaderT :: Get DHeaderT
getDHeaderT = DHeaderT <$> getInt <*> getInt <*> getLumps
  where getLumps = V.replicateM Constants.headerLumps getLumpT