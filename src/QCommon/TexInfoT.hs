{-# LANGUAGE TemplateHaskell #-}
module QCommon.TexInfoT
    ( module QCommon.TexInfoT
    ) where

import           Control.Lens    (makeLenses)
import           Data.Binary.Get (Get, getByteString)
import qualified Data.ByteString as B

import           Types
import           Util.Binary     (getV4Float, getInt)

texInfoTSize :: Int
texInfoTSize = 32 + 4 + 4 + 32 + 4

makeLenses ''TexInfoT

getTexInfoT :: Get TexInfoT
getTexInfoT = TexInfoT <$> ((,) <$> getV4Float <*> getV4Float)
                       <*> getInt
                       <*> getInt
                       <*> (B.takeWhile (/= 0) <$> getByteString 32)
                       <*> getInt