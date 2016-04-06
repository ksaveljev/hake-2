{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DMdlT
  ( module QCommon.QFiles.MD2.DMdlT
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B

makeLenses ''DMdlT

idAliasHeader :: B.ByteString
idAliasHeader = "IDP2"
