{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DMdlT where

import           Control.Applicative ((<*>))
import           Control.Lens (makeLenses)
import           Data.Functor ((<$>))
import           Data.Int (Int32)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Data.Word (Word32)

import           Types
import           Util.Binary

idAliasHeader :: B.ByteString
idAliasHeader = "IDP2"

makeLenses ''DMdlT

newDMdlT :: BL.ByteString -> DMdlT
newDMdlT = runGet getDMdlT
  where getDMdlT :: Get DMdlT
        getDMdlT = DMdlT <$> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> getInt
                         <*> return Nothing
                         <*> return Nothing
                         <*> return Nothing
                         <*> return Nothing
                         <*> return Nothing
                         <*> return 0
                         <*> return 0
                         <*> return UV.empty
                         <*> return V.empty
