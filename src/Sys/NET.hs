{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module Sys.NET where

import Control.Concurrent (threadDelay)
import Control.Exception (handle, IOException)
import Control.Lens (preuse, use, (^.), (.=), Lens', (+=), ix)
import Control.Monad (when, liftM, unless)
import Data.Bits ((.&.))
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, isNothing)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.BSD as NBSD
import qualified Network.Socket as NS

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified Util.Lib as Lib

maxLoopback :: Int
maxLoopback = 4

-- local loopback address
netLocalAdr :: NetAdrT
netLocalAdr = newNetAdrT

init :: Quake ()
init = return () -- nothing to do

-- Config multi or singlepalyer - A single player game will only use the loopback code.
config :: Bool -> Quake ()
config multiplayer =
    if multiplayer
      then openIP
      else do
        -- shut down any existing sockets
        ipSocketClient <- use $ netGlobals.ngIpSocketClient
        ipSocketServer <- use $ netGlobals.ngIpSocketServer

        when (isJust (ipSocketClient)) $ do
          io $ NS.close (fromJust $ ipSocketClient) -- IMPROVE: catch exceptions if any?
          netGlobals.ngIpSocketClient .= Nothing

        when (isJust (ipSocketServer)) $ do
          io $ NS.close (fromJust $ ipSocketServer) -- IMPROVE: catch exceptions if any?
          netGlobals.ngIpSocketServer .= Nothing

openIP :: Quake ()
openIP = do
    Just port <- CVar.get "port" (BC.pack $ show Constants.portServer) Constants.cvarNoSet -- IMPROVE: convert Int to ByteString using binary package?
    Just ip <- CVar.get "ip" "localhost" Constants.cvarNoSet
    Just clientport <- CVar.get "clientport" (BC.pack $ show Constants.portClient) Constants.cvarNoSet -- IMPROVE: convert Int to ByteString using binary package?

    ipSocketClient <- use $ netGlobals.ngIpSocketClient
    ipSocketServer <- use $ netGlobals.ngIpSocketServer

    when (isNothing (ipSocketServer)) $ do
      s <- socket (ip^.cvString) (truncate (port^.cvValue))
      netGlobals.ngIpSocketServer .= s

    when (isNothing (ipSocketClient)) $ do
      s <- socket (ip^.cvString) (truncate (clientport^.cvValue))
      if isNothing s
        then do
          newS <- socket (ip^.cvString) (Constants.portAny)
          netGlobals.ngIpSocketClient .= newS
        else
          netGlobals.ngIpSocketClient .= s

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
      then
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

-- Gets a packet from a network channel
getPacket :: Int -> Lens' QuakeState NetAdrT -> Lens' QuakeState SizeBufT -> Quake Bool
getPacket sock netFromLens netMessageLens = do
    done <- if sock == Constants.nsClient
              then getLoopPacket (netGlobals.ngLoopbackClient) netFromLens netMessageLens
              else getLoopPacket (netGlobals.ngLoopbackServer) netFromLens netMessageLens

    if sock == Constants.nsClient
      then getNetworkPacket done (netGlobals.ngIpSocketClient)
      else getNetworkPacket done (netGlobals.ngIpSocketServer)

  where getNetworkPacket :: Bool -> Lens' QuakeState (Maybe NS.Socket) -> Quake Bool
        getNetworkPacket done socketLens = do
          s <- use socketLens
          if | done -> return True
             | isNothing s -> return False
             | otherwise -> do
                 return False -- io (putStrLn "NET.getPacket") >> undefined -- TODO

-- Compares ip address without the port
compareBaseAdr :: NetAdrT -> NetAdrT -> Bool
compareBaseAdr a b =
    if | (a^.naType) /= (b^.naType) -> False
       | (a^.naType) == Constants.naLoopback -> True
       | (a^.naType) == Constants.naIp -> (a^.naIP) == (b^.naIP) -- TODO: verify it works?
       | otherwise -> False

-- Gets a packet from internal loopback.
getLoopPacket :: Lens' QuakeState LoopbackT -> Lens' QuakeState NetAdrT -> Lens' QuakeState SizeBufT -> Quake Bool
getLoopPacket loopbackLens netFromLens netMessageLens = do
    checkLoopbackSendGet

    loop <- use loopbackLens

    if (loop^.lGet >= loop^.lSend)
      then return False
      else do
        let idx = (loop^.lGet) .&. (maxLoopback - 1)
        loopbackLens.lGet += 1

        Just msg <- preuse $ loopbackLens.lMsgs.ix idx
        let buf = B.take (msg^.lmDataLen) (msg^.lmData)
        netMessageLens.sbData .= buf
        netMessageLens.sbCurSize .= (msg^.lmDataLen)

        netFromLens .= netLocalAdr
        return True

  where checkLoopbackSendGet :: Quake ()
        checkLoopbackSendGet = do
          loop <- use loopbackLens

          when ((loop^.lSend) - (loop^.lGet) > maxLoopback) $
            loopbackLens.lGet .= (loop^.lSend) - maxLoopback

-- Sleeps msec or until net socket is ready
sleep :: Int -> Quake ()
sleep msec = do
    ipSocketServer <- use $ netGlobals.ngIpSocketServer
    dedicatedValue <- liftM (^.cvValue) dedicatedCVar

    -- when we are not a server, just run full speed
    unless (isNothing ipSocketServer || dedicatedValue == 0) $
      io (threadDelay $ msec * 1000)
