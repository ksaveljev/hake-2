module Server.SVInit where

import qualified Data.ByteString as B

import Quake

findIndex :: B.ByteString -> Int -> Int -> Bool -> Quake Int

modelIndex :: B.ByteString -> Quake Int

soundIndex :: B.ByteString -> Quake Int

imageIndex :: B.ByteString -> Quake Int

createBaseline :: Quake ()

checkForSavegame :: Quake ()

spawnServer :: B.ByteString -> B.ByteString -> Int -> Bool -> Bool -> Quake ()

initGame :: Quake ()

svMap :: Bool -> B.ByteString -> Bool -> Quake ()
