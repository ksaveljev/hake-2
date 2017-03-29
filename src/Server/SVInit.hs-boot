module Server.SVInit where

import qualified Data.ByteString as B

import Types

findIndex :: Maybe B.ByteString -> Int -> Int -> Bool -> Quake Int

modelIndex :: Maybe B.ByteString -> Quake Int

soundIndex :: Maybe B.ByteString -> Quake Int

imageIndex :: Maybe B.ByteString -> Quake Int

createBaseline :: Quake ()

checkForSavegame :: Quake ()

spawnServer :: B.ByteString -> B.ByteString -> Int -> Bool -> Bool -> Quake ()

initGame :: Quake ()

svMap :: Bool -> B.ByteString -> Bool -> Quake ()
