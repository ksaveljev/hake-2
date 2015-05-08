{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.SP2.DSprFrameT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Control.Monad (liftM)
import Data.Functor ((<$>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import Util.Binary

data DSprFrameT =
  DSprFrameT { _dsfWidth   :: Int
             , _dsfHeight  :: Int
             , _dsfOriginX :: Int
             , _dsfOriginY :: Int
             , _dsfName    :: B.ByteString
             }

makeLenses ''DSprFrameT

newDSprFrameT :: BL.ByteString -> DSprFrameT
newDSprFrameT = runGet getDSprFrameT

getDSprFrameT :: Get DSprFrameT
getDSprFrameT = DSprFrameT <$> getInt
                           <*> getInt
                           <*> getInt
                           <*> getInt
                           <*> liftM (B.reverse . BC.dropWhile (<= ' ') . B.reverse . BL.toStrict) getRemainingLazyByteString
