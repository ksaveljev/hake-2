module Server.SVInit
  ( imageIndex
  , modelIndex
  , soundIndex
  , svMap
  ) where

import qualified Constants
import           Types

import qualified Data.ByteString as B

svMap :: Bool -> B.ByteString -> Bool -> Quake ()
svMap = error "SVInit.svMap" -- TODO

findIndex :: Maybe B.ByteString -> Int -> Int -> Bool -> Quake Int
findIndex = error "SVInit.findIndex" -- TODO

modelIndex :: Maybe B.ByteString -> Quake Int
modelIndex name = findIndex name Constants.csModels Constants.maxModels True

soundIndex :: Maybe B.ByteString -> Quake Int
soundIndex name = findIndex name Constants.csSounds Constants.maxSounds True

imageIndex :: Maybe B.ByteString -> Quake Int
imageIndex name = findIndex name Constants.csImages Constants.maxImages True
