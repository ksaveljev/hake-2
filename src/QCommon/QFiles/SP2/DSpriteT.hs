{-# LANGUAGE TemplateHaskell #-}
module QCommon.QFiles.SP2.DSpriteT where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Types
import QCommon.QFiles.SP2.DSprFrameT
import Util.Binary

idSpriteHeader :: B.ByteString
idSpriteHeader = "IDS2"

spriteVersion :: Int
spriteVersion = 2

makeLenses ''DSpriteT

newDSpriteT :: BL.ByteString -> DSpriteT
newDSpriteT = runGet getDSpriteT
  where getDSpriteT :: Get DSpriteT
        getDSpriteT = do
          ident <- getInt
          version <- getInt
          numFrames <- getInt
          frames <- getFrames numFrames
          return $ DSpriteT ident version numFrames frames

        getFrames :: Int -> Get (V.Vector DSprFrameT)
        getFrames numFrames = V.replicateM numFrames getDSprFrameT
