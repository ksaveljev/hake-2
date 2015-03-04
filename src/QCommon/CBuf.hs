module QCommon.CBuf where

import Control.Lens
import Control.Monad.State

import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import qualified QCommon.SZ as SZ

init :: Quake ()
init = do
    bufData <- liftM (^.globals.cmdTextBuf) get
    SZ.init bufData (UV.length bufData) (globals.cmdText .=)

addEarlyCommands :: Bool -> Quake ()
addEarlyCommands clear = undefined -- TODO

execute :: Quake ()
execute = undefined -- TODO
