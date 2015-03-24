{-# LANGUAGE OverloadedStrings #-}
module Sys.NET where

import Data.Maybe (isJust, fromJust, isNothing)
import Control.Lens (use, (^.), _1, _2, (.=))
import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket as NS

import Quake
import QuakeState
import QCommon.NetAdrT
import qualified Constants
import qualified QCommon.CVar as CVar

init :: Quake ()
init = return () -- nothing to do

-- Config multi or singlepalyer - A single player game will only use the loopback code.
config :: Bool -> Quake ()
config multiplayer =
    if multiplayer
      then openIP
      else do
        ipSockets <- use $ netGlobals.ngIpSockets
        when (isJust (ipSockets^._1)) $
          io $ NS.close (fromJust $ ipSockets^._1) -- IMPROVE: catch exceptions if any?
        when (isJust (ipSockets^._2)) $
          io $ NS.close (fromJust $ ipSockets^._2) -- IMPROVE: catch exceptions if any?
        netGlobals.ngIpSockets .= (Nothing, Nothing)

openIP :: Quake ()
openIP = do
    Just port <- CVar.get "port" (BC.pack $ show Constants.portServer) Constants.cvarNoSet -- IMPROVE: convert Int to ByteString using binary package?
    Just ip <- CVar.get "ip" "localhost" Constants.cvarNoSet
    Just clientport <- CVar.get "clientport" (BC.pack $ show Constants.portClient) Constants.cvarNoSet -- IMPROVE: convert Int to ByteString using binary package?

    ipSockets <- use $ netGlobals.ngIpSockets

    when (isNothing (ipSockets^._2)) $ do -- _2 corresponds to Constants.nsServer
      s <- socket (ip^.cvString) (truncate (port^.cvValue))
      netGlobals.ngIpSockets._2 .= s
    when (isNothing (ipSockets^._1)) $ do -- _1 corresponds to Constants.nsClient
      s <- socket (ip^.cvString) (truncate (clientport^.cvValue))
      if isNothing s
        then do
          newS <- socket (ip^.cvString) (Constants.portAny)
          netGlobals.ngIpSockets._1 .= newS
        else
          netGlobals.ngIpSockets._1 .= s

socket :: B.ByteString -> Int -> Quake (Maybe NS.Socket)
socket _ _ = io (putStrLn "NET.socket") >> undefined -- TODO

-- Creates an netadr_t from a string
stringToAdr :: B.ByteString -> Quake (Maybe NetAdrT)
stringToAdr _ = io (putStrLn "NET.stringToAdr") >> undefined -- TODO

-- Returns a string holding ip address and port like "ip0.ip1.ip2.ip3:port".
adrToString :: NetAdrT -> B.ByteString
adrToString _ = undefined -- TODO -- putStrLn for grep
