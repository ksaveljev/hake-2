module Client.CLEnts where

import Control.Lens (use)

import Quake
import QuakeState
import qualified QCommon.MSG as MSG

addEntities :: Quake ()
addEntities = do
    io (putStrLn "CLEnts.addEntities") >> undefined -- TODO

{-
- ================= CL_ParseEntityBits
- 
- Returns the entity number and the header bits =================
-}
parseEntityBits :: [Int] -> Quake (Int, [Int])
parseEntityBits bits = do
    total <- MSG.readByte (globals.netMessage)
    undefined -- TODO
