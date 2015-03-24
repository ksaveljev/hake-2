{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sys.NET where

import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, isNothing)
import Control.Lens (use, (^.), _1, _2, (.=))
import Control.Monad (when)
import Control.Exception (handle, IOException)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.BSD as NBSD
import qualified Network.Socket as NS

import Quake
import QuakeState
import QCommon.NetAdrT
import qualified Constants
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified Util.Lib as Lib

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
socket ip port = do
    openedSocket <-
      io $ handle (\(e :: IOException) -> return (Left e)) $ do
        newsocket <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
        NS.setSocketOption newsocket NS.Broadcast 1
        return (Right newsocket)

    case openedSocket of
      Left e -> do
        Com.println $ "Error: " `B.append` (BC.pack $ show e)
        return Nothing
      Right s -> do
        bindResult <- io $ handle (\(e :: IOException) -> return (Left e)) $ do
          if B.length ip == 0 || ip == "localhost"
            then if port == Constants.portAny
                   then NS.bind s (NS.SockAddrInet (NS.PortNum 0) NS.iNADDR_ANY)
                   else NS.bind s (NS.SockAddrInet (NS.PortNum (fromIntegral port)) NS.iNADDR_ANY)
            else do
              resolved <- NBSD.getHostByName (BC.unpack ip)
              NS.bind s (NS.SockAddrInet (NS.PortNum (fromIntegral port)) (head $ NBSD.hostAddresses resolved)) -- this head is kinda bad, isn't it?
          return $ Right () 

        case bindResult of
          Left e -> do
            io $ NS.close s
            Com.println $ "Error: " `B.append` (BC.pack $ show e)
            return Nothing
          Right _ ->
            return $ Just s

-- Creates an netadr_t from a string
stringToAdr :: B.ByteString -> Quake (Maybe NetAdrT)
stringToAdr s = do
    let sLow = BC.map toLower s
        address = BC.split ':' s

    if sLow == "localhost" || sLow == "loopback"
      then do
        netLocalAdr <- use $ netGlobals.ngNetLocalAdr
        return $ Just netLocalAdr
      else do
        parsedData <- io $ handle (\(e :: IOException) -> return $ Left e) $ do
          resolved <- NBSD.getHostByName (BC.unpack $ head address)
          let ip = head $ NBSD.hostAddresses resolved -- IMPROVE: this is a bad head, isn't it?
          return $ Right ip

        case parsedData of
          Left e -> do
            Com.println $ BC.pack (show e)
            return Nothing
          Right ip -> if length address == 2
                        then let port = Lib.atoi (last address)
                             in return $ Just newNetAdrT { _naIP = Just ip, _naType = Constants.naIp, _naPort = port }
                        else return $ Just newNetAdrT { _naIP = Just ip, _naType = Constants.naIp }

-- Returns a string holding ip address and port like "ip0.ip1.ip2.ip3:port".
adrToString :: NetAdrT -> B.ByteString
adrToString _ = undefined -- TODO -- putStrLn for grep
