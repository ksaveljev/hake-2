module Client.M
    ( catagorizePosition
    , changeYaw
    , checkBottom
    , checkGround
    , dropToFloor
    , flyCheck
    , moveFrame
    , moveToGoal
    , setEffects
    , walkMove
    , worldEffects
    ) where

import           Types

checkGround :: Ref EdictT -> Quake ()
dropToFloor :: EntThink
walkMove :: Ref EdictT -> Float -> Float -> Quake Bool
catagorizePosition :: Ref EdictT -> Quake ()
flyCheck :: EntThink
checkBottom :: Ref EdictT -> Quake Bool
setEffects :: Ref EdictT -> Quake ()
worldEffects :: Ref EdictT -> Quake ()
moveFrame :: Ref EdictT -> Quake ()
changeYaw :: Ref EdictT -> Quake ()
moveToGoal :: Ref EdictT -> Float -> Quake ()
