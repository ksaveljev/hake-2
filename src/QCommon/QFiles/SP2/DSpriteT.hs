{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.SP2.DSpriteT
    ( module QCommon.QFiles.SP2.DSpriteT
    ) where

import           Control.Lens                  (makeLenses)
import           Data.Binary.Get               (Get)
import qualified Data.ByteString               as B
import qualified Data.Vector                   as V

import           QCommon.QFiles.SP2.DSprFrameT
import           Types
import           Util.Binary                   (getInt)

makeLenses ''DSpriteT

spriteVersion :: Int
spriteVersion = 2

idSpriteHeader :: B.ByteString
idSpriteHeader = "IDS2"

getDSpriteT :: Get DSpriteT
getDSpriteT = do
    ident <- getInt
    version <- getInt
    numFrames <- getInt
    frames <- V.replicateM numFrames getDSprFrameT
    return (DSpriteT ident version numFrames frames)