{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.SP2.DSprFrameT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Control.Monad (liftM)
import Data.Functor ((<$>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import Types
import Util.Binary
import qualified Constants

makeLenses ''DSprFrameT

newDSprFrameT :: BL.ByteString -> DSprFrameT
newDSprFrameT = runGet getDSprFrameT

getDSprFrameT :: Get DSprFrameT
getDSprFrameT = DSprFrameT <$> getInt
                           <*> getInt
                           <*> getInt
                           <*> getInt
                           <*> liftM (B.reverse . BC.dropWhile (<= ' ') . B.reverse) (getByteString Constants.maxSkinName)
