module QCommon.Netchan where

import Data.Bits ((.&.))

import Quake
import qualified Sys.Timer as Timer

init :: Quake ()
init = do
    msec <- Timer.milliseconds
    
    let port = msec .&. 0xFFFF

    undefined -- TODO
