module Client.M
    ( checkBottom
    , checkGround
    , dropToFloor
    , flyCheck
    , walkMove
    ) where

import           Types

checkBottom :: Ref EdictT -> Quake Bool
checkGround :: Ref EdictT -> Quake ()
dropToFloor :: EntThink
walkMove :: Ref EdictT -> Float -> Float -> Quake Bool
flyCheck :: EntThink
