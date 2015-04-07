{-# LANGUAGE Rank2Types #-}
module QCommon.MSG where

import Control.Lens (ASetter')
import Linear.V3 (V3)
import qualified Data.ByteString as B

import Quake
import QuakeState

writeCharI :: ASetter' QuakeState SizeBufT -> Int -> Quake ()
writeCharI _ _ = io (putStrLn "MSG.writeCharI") >> undefined -- TODO

writeCharF :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeCharF _ _ = io (putStrLn "MSG.writeCharF") >> undefined -- TODO

writeByteI :: ASetter' QuakeState SizeBufT -> Int -> Quake ()
writeByteI _ _ = io (putStrLn "MSG.writeByteI") >> undefined -- TODO

writeByteF :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeByteF _ _ = io (putStrLn "MSG.writeByteF") >> undefined -- TODO

writeShort :: ASetter' QuakeState SizeBufT -> Int -> Quake ()
writeShort _ _ = io (putStrLn "MSG.writeShort") >> undefined -- TODO

writeInt :: ASetter' QuakeState SizeBufT -> Int -> Quake ()
writeInt _ _ = io (putStrLn "MSG.writeInt") >> undefined -- TODO

writeLong :: ASetter' QuakeState SizeBufT -> Int -> Quake ()
writeLong _ _ = io (putStrLn "MSG.writeLong") >> undefined -- TODO

writeFloat :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeFloat _ _ = io (putStrLn "MSG.writeFloat") >> undefined -- TODO

writeString :: ASetter' QuakeState SizeBufT -> B.ByteString -> Quake ()
writeString _ _ = io (putStrLn "MSG.writeString") >> undefined -- TODO

writeCoord :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeCoord _ _ = io (putStrLn "MSG.writeCoord") >> undefined -- TODO

writePos :: ASetter' QuakeState SizeBufT -> V3 Float -> Quake ()
writePos _ _ = io (putStrLn "MSG.writePos") >> undefined -- TODO

writeDir :: ASetter' QuakeState SizeBufT -> V3 Float -> Quake ()
writeDir _ _ = io (putStrLn "MSG.writeDir") >> undefined -- TODO

writeAngle :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeAngle _ _ = io (putStrLn "MSG.writeAngle") >> undefined -- TODO

writeAngle16 :: ASetter' QuakeState SizeBufT -> Float -> Quake ()
writeAngle16 _ _ = io (putStrLn "MSG.writeAngle16") >> undefined -- TODO
