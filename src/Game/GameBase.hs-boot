{-# LANGUAGE Rank2Types #-}
module Game.GameBase where

import Linear (V3)
import qualified Data.ByteString as B

import Types
import QuakeState

shutdownGame :: Quake ()

runFrame :: Quake ()

getGameApi :: GameImportT -> Quake ()

findByTarget :: EdictT -> B.ByteString -> Bool

findByClass :: EdictT -> B.ByteString -> Bool

gFind :: Maybe EdictReference -> (EdictT -> B.ByteString -> Bool) -> B.ByteString -> Quake (Maybe EdictReference)

setMoveDir :: EdictReference -> Quake ()

exitLevel :: Quake ()

checkDMRules :: Quake ()

checkNeedPass :: Quake ()

clientEndServerFrames :: Quake ()

runEntity :: EdictReference -> Quake ()

pickTarget :: Maybe B.ByteString -> Quake (Maybe EdictReference)

addPointToBound :: V3 Float -> V3 Float -> V3 Float -> (V3 Float, V3 Float)

clipVelocity :: V3 Float -> V3 Float -> Float -> (Int, V3 Float)

touchTriggers :: EdictReference -> Quake ()

findRadius :: Maybe EdictReference -> V3 Float -> Float -> Quake (Maybe EdictReference)
