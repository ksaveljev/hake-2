{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.SP2.DSpriteT
  ( module QCommon.QFiles.SP2.DSpriteT
  ) where

import           Types

import           Control.Lens (makeLenses)
import qualified Data.ByteString as B

makeLenses ''DSpriteT

idSpriteHeader :: B.ByteString
idSpriteHeader = "IDS2"
