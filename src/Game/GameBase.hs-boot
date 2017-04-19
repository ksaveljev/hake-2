module Game.GameBase
    ( findByClass
    , findByTarget
    , getGameApi
    , gFind
    , runFrame
    , setMoveDir
    , shutdownGame
    ) where

import qualified Data.ByteString as B

import           Types

findByClass :: EdictT -> B.ByteString -> Bool
findByTarget :: EdictT -> B.ByteString -> Bool
getGameApi :: GameImportT -> Quake ()
gFind :: Maybe (Ref EdictT) -> (EdictT -> B.ByteString -> Bool) -> B.ByteString -> Quake (Maybe (Ref EdictT))
runFrame :: Quake ()
setMoveDir :: Ref EdictT -> EdictT -> Quake ()
shutdownGame :: Quake ()
