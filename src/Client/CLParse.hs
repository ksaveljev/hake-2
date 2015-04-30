module Client.CLParse where

import Quake
import QCommon.XCommandT

downloadF :: XCommandT
downloadF = io (putStrLn "CLParse.downloadF") >> undefined -- TODO

parseServerMessage :: Quake ()
parseServerMessage = io (putStrLn "CLParse.parseServerMessage") >> undefined -- TODO
