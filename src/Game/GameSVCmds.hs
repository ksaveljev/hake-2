module Game.GameSVCmds
  ( filterPacket
  , serverCommand
  ) where

import           Types

import qualified Data.ByteString as B

serverCommand :: Quake ()
serverCommand = error "GameSVCmds.serverCommand" -- TODO

filterPacket :: B.ByteString -> Quake Bool
filterPacket _ = request (io (putStrLn "GameSVCmds.filterPacket IMPLEMENT ME!")) >> return False -- TODO