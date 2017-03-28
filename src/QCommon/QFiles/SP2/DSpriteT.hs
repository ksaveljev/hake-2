{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module QCommon.QFiles.SP2.DSpriteT ( module QCommon.QFiles.SP2.DSpriteT
                                   , module QCommon.QFiles.SP2.DSprFrameT
                                   ) where

import Control.Lens (makeLenses)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import QCommon.QFiles.SP2.DSprFrameT
import Util.Binary

idSpriteHeader :: B.ByteString
idSpriteHeader = "IDS2"

spriteVersion :: Int
spriteVersion = 2

data DSpriteT =
  DSpriteT { _dsIdent     :: Int
           , _dsVersion   :: Int
           , _dsNumFrames :: Int
           , _dsFrames    :: V.Vector DSprFrameT
           }

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
