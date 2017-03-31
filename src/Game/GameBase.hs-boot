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

gFind :: Maybe (Ref EdictT) -> (EdictT -> B.ByteString -> Bool) -> B.ByteString -> Quake (Maybe (Ref EdictT))

setMoveDir :: Ref EdictT -> Quake ()

exitLevel :: Quake ()

checkDMRules :: Quake ()

checkNeedPass :: Quake ()

clientEndServerFrames :: Quake ()

runEntity :: Ref EdictT -> Quake ()

pickTarget :: Maybe B.ByteString -> Quake (Maybe (Ref EdictT))

addPointToBound :: V3 Float -> V3 Float -> V3 Float -> (V3 Float, V3 Float)

clipVelocity :: V3 Float -> V3 Float -> Float -> (Int, V3 Float)

touchTriggers :: Ref EdictT -> Quake ()

findRadius :: Maybe (Ref EdictT) -> V3 Float -> Float -> Quake (Maybe (Ref EdictT))
