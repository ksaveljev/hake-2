{-# LANGUAGE Rank2Types #-}
module QCommon.MSG where

import Linear.V3 (V3)
import qualified Data.ByteString as B

import Quake
import QuakeState

writeCharI :: QuakeLens SizeBufT -> Int -> Quake ()
writeCharI _ _ = io (putStrLn "MSG.writeCharI") >> undefined -- TODO

writeCharF :: QuakeLens SizeBufT -> Float -> Quake ()
writeCharF _ _ = io (putStrLn "MSG.writeCharF") >> undefined -- TODO

writeByteI :: QuakeLens SizeBufT -> Int -> Quake ()
writeByteI _ _ = io (putStrLn "MSG.writeByteI") >> undefined -- TODO

writeByteF :: QuakeLens SizeBufT -> Float -> Quake ()
writeByteF _ _ = io (putStrLn "MSG.writeByteF") >> undefined -- TODO

writeShort :: QuakeLens SizeBufT -> Int -> Quake ()
writeShort _ _ = io (putStrLn "MSG.writeShort") >> undefined -- TODO

writeInt :: QuakeLens SizeBufT -> Int -> Quake ()
writeInt _ _ = io (putStrLn "MSG.writeInt") >> undefined -- TODO

writeLong :: QuakeLens SizeBufT -> Int -> Quake ()
writeLong _ _ = io (putStrLn "MSG.writeLong") >> undefined -- TODO

writeFloat :: QuakeLens SizeBufT -> Float -> Quake ()
writeFloat _ _ = io (putStrLn "MSG.writeFloat") >> undefined -- TODO

writeString :: QuakeLens SizeBufT -> B.ByteString -> Quake ()
writeString _ _ = io (putStrLn "MSG.writeString") >> undefined -- TODO

writeCoord :: QuakeLens SizeBufT -> Float -> Quake ()
writeCoord _ _ = io (putStrLn "MSG.writeCoord") >> undefined -- TODO

writePos :: QuakeLens SizeBufT -> V3 Float -> Quake ()
writePos _ _ = io (putStrLn "MSG.writePos") >> undefined -- TODO

writeAngle :: QuakeLens SizeBufT -> Float -> Quake ()
writeAngle _ _ = io (putStrLn "MSG.writeAngle") >> undefined -- TODO

writeAngle16 :: QuakeLens SizeBufT -> Float -> Quake ()
writeAngle16 _ _ = io (putStrLn "MSG.writeAngle16") >> undefined -- TODO
