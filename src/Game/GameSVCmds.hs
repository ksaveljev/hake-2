module Game.GameSVCmds where

import qualified Data.ByteString as B

import Quake

serverCommand :: Quake ()
serverCommand = io (putStrLn "GameSVCmds.serverCommand") >> undefined -- TODO

filterPacket :: B.ByteString -> Quake Bool
filterPacket _ = do
    io (putStrLn "GameSVCmds.filterPacket") >> undefined -- TODO
