module Client.M
    ( checkGround
    , dropToFloor
    , walkMove
    ) where

import Types

checkGround :: Ref' EdictT -> Quake ()
checkGround = error "M.checkGround" -- TODO

dropToFloor :: EntThink
dropToFloor = EntThink "m_drop_to_floor" $ \edictRef -> do
    error "M.dropToFloor" -- TODO

walkMove :: Ref' EdictT -> Float -> Float -> Quake Bool
walkMove = error "M.walkMove" -- TODO