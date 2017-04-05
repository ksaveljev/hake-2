{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Game.GameSVCmds where

import Control.Lens (use)
import Control.Monad (liftM)
import Data.Char (toLower)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import {-# SOURCE #-} Game.GameImportT
import Types
import QuakeState
import qualified Constants

{-
- ServerCommand
- 
- ServerCommand will be called when an "sv" command is issued. The game can
- issue gi.argc() / gi.argv() commands to get the rest of the parameters
-}
serverCommand :: Quake ()
serverCommand = do
    argv <- use $ gameBaseGlobals.gbGameImport.giArgv
    cmd <- liftM (BC.map toLower) (argv 1)
    
    if | cmd == "test" -> svcmdTestF
       | cmd == "addip" -> svcmdAddIpF
       | cmd == "removeip" -> svcmdRemoveIpF
       | cmd == "listip" -> svcmdListIpF
       | cmd == "writeip" -> svcmdWriteIpF
       | otherwise -> do
           cprintf <- use $ gameBaseGlobals.gbGameImport.giCprintf
           cprintf Nothing Constants.printHigh ("Unknown server command \"" `B.append` cmd `B.append` "\"\n")

filterPacket :: B.ByteString -> Quake Bool
filterPacket _ = do
    io (putStrLn "IMPLEMENT ME! SKIPPING IP FILTERING") >> return False -- TODO
    -- io (putStrLn "GameSVCmds.filterPacket") >> undefined -- TODO

svcmdTestF :: Quake ()
svcmdTestF = do
    cprintf <- use $ gameBaseGlobals.gbGameImport.giCprintf
    cprintf Nothing Constants.printHigh "Svcmd_Test_f()\n"

svcmdAddIpF :: Quake ()
svcmdAddIpF = do
    io (putStrLn "GameSVCmds.svcmdAddIpF") >> undefined -- TODO

svcmdRemoveIpF :: Quake ()
svcmdRemoveIpF = do
    io (putStrLn "GameSVCmds.svcmdRemoveIpF") >> undefined -- TODO

svcmdListIpF :: Quake ()
svcmdListIpF = do
    io (putStrLn "GameSVCmds.svcmdListIpF") >> undefined -- TODO
    
svcmdWriteIpF :: Quake ()
svcmdWriteIpF = do
    io (putStrLn "GameSVCmds.svcmdWriteIpF") >> undefined -- TODO