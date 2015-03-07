module QCommon.CBuf where

import Control.Lens (use, (.=))

import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import qualified QCommon.SZ as SZ

init :: Quake ()
init = do
    bufData <- use $ globals.cmdTextBuf
    SZ.init bufData (UV.length bufData) (globals.cmdText .=)

addEarlyCommands :: Bool -> Quake ()
addEarlyCommands clear = do
    --c <- Com.argc
    undefined -- TODO

execute :: Quake ()
execute = undefined -- TODO
