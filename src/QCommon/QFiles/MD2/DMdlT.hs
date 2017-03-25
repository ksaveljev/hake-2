{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DMdlT
    ( module QCommon.QFiles.MD2.DMdlT
    ) where

import           Control.Lens        (makeLenses)
import           Data.Binary.Get     (Get)
import qualified Data.ByteString     as B
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV

import           Types
import           Util.Binary         (getInt)

makeLenses ''DMdlT

idAliasHeader :: B.ByteString
idAliasHeader = "IDP2"

getDMdlT :: Get DMdlT
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