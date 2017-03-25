{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.SP2.DSprFrameT
    ( module QCommon.QFiles.SP2.DSprFrameT
    ) where

import           Control.Lens          (makeLenses)
import           Data.Binary.Get       (Get, getByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC

import qualified Constants
import           Types
import           Util.Binary           (getInt)

makeLenses ''DSprFrameT

getDSprFrameT :: Get DSprFrameT
getDSprFrameT = DSprFrameT <$> getInt
                           <*> getInt
                           <*> getInt
                           <*> getInt
                           <*> fmap (B.reverse . BC.dropWhile (<= ' ') . B.reverse) (getByteString Constants.maxSkinName)