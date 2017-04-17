module Server.SVInit
    ( imageIndex
    , modelIndex
    , soundIndex
    , svMap
    ) where

import qualified Data.ByteString as B

import           Types

imageIndex :: Maybe B.ByteString -> Quake Int
modelIndex :: Maybe B.ByteString -> Quake Int
soundIndex :: Maybe B.ByteString -> Quake Int
svMap :: Bool -> B.ByteString -> Bool -> Quake ()