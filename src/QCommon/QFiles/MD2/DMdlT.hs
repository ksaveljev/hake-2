{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.MD2.DMdlT where

import Control.Applicative ((<*>))
import Control.Lens (makeLenses)
import Data.Binary.Get
import Data.Functor ((<$>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import QCommon.QFiles.MD2.DAliasFrameT
import QCommon.QFiles.MD2.DSTVertT
import QCommon.QFiles.MD2.DTriangleT

data DMdlT =
  DMdlT { _dmIdent       :: Int
        , _dmVersion     :: Int
        , _dmSkinWidth   :: Int
        , _dmSkinHeight  :: Int
        , _dmFrameSize   :: Int
        , _dmNumSkins    :: Int
        , _dmNumXYZ      :: Int
        , _dmNumST       :: Int
        , _dmNumTris     :: Int
        , _dmNumGlCmds   :: Int
        , _dmNumFrames   :: Int
        , _dmOfsSkins    :: Int
        , _dmOfsST       :: Int
        , _dmOfsTris     :: Int
        , _dmOfsFrames   :: Int
        , _dmOfsGlCmds   :: Int
        , _dmOfsEnd      :: Int
        , _dmSkinNames   :: Maybe (V.Vector B.ByteString)
        , _dmSTVerts     :: Maybe (V.Vector DSTVertT)
        , _dmTriAngles   :: Maybe (V.Vector DTriangleT)
        , _dmGlCmds      :: Maybe (UV.Vector Int)
        , _dmAliasFrames :: Maybe (V.Vector DAliasFrameT)
        }

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

        getInt :: Get Int
        getInt = fromIntegral <$> getWord32le
