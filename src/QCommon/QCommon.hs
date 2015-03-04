module QCommon.QCommon where

import Quake (Quake, io)
import qualified QCommon.Com as Com

init :: [String] -> Quake ()
init args = do
    io $ putStrLn "QCommon.init"
    Com.initArgv args
    undefined

frame :: Int -> Quake ()
frame = undefined -- TODO

reconfigure :: Bool -> Quake ()
reconfigure = undefined
