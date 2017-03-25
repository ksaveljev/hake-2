module Game.GameSVCmds
    ( filterPacket
    , serverCommand
    ) where

import qualified Data.ByteString as B

import           Types

serverCommand :: Quake ()
serverCommand = error "GameSVCmds.serverCommand" -- TODO

filterPacket :: B.ByteString -> Quake Bool
filterPacket _ = io (putStrLn "GameSVCmds.filterPacket IMPLEMENT ME!") >> return False -- TODO