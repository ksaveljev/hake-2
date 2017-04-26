module Game.GameBase
    ( addPointToBounds
    , clipVelocity
    , findByClass
    , findByTarget
    , getGameApi
    , gFind
    , pickTarget
    , runFrame
    , setMoveDir
    , shutdownGame
    , touchTriggers
    ) where

import qualified Data.ByteString as B
import           Linear          (V3)

import           Types

addPointToBounds :: V3 Float -> V3 Float -> V3 Float -> (V3 Float, V3 Float)
clipVelocity :: V3 Float -> V3 Float -> Float -> (Int, V3 Float)
findByClass :: EdictT -> B.ByteString -> Bool
findByTarget :: EdictT -> B.ByteString -> Bool
getGameApi :: GameImportT -> Quake ()
gFind :: Maybe (Ref EdictT) -> (EdictT -> B.ByteString -> Bool) -> B.ByteString -> Quake (Maybe (Ref EdictT))
pickTarget :: Maybe B.ByteString -> Quake (Maybe (Ref EdictT))
runFrame :: Quake ()
setMoveDir :: Ref EdictT -> EdictT -> Quake ()
shutdownGame :: Quake ()
touchTriggers :: Ref EdictT -> Quake ()
