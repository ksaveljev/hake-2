{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
module Sys.NET where

import Control.Concurrent (threadDelay)
import Control.Exception (handle, IOException)
import Control.Lens (preuse, use, (^.), (.=), Lens', (+=), ix, zoom)
import Control.Monad (when, liftM, unless, void)
import Data.Bits ((.&.), shiftR)
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, isNothing)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.BSD as NBSD
import qualified Network.Socket as NS

import QCommon.SizeBufT
import Sys.LoopbackT
import Sys.LoopMsgT
import Types
import QuakeState
import CVarVariables
import QCommon.NetAdrT
import qualified Constants
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified Sys.Socket as S
import qualified Util.Lib as Lib

maxLoopback :: Int
maxLoopback = 4

-- local loopback address
netLocalAdr :: NetAdrT
netLocalAdr = newNetAdrT

initialize :: Quake ()
initialize = return () -- nothing to do

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
          io $ S.close (fromJust $ ipSocketClient) -- IMPROVE: catch exceptions if any?
          netGlobals.ngIpSocketClient .= Nothing

        when (isJust (ipSocketServer)) $ do
          io $ S.close (fromJust $ ipSocketServer) -- IMPROVE: catch exceptions if any?
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

socket :: B.ByteString -> Int -> Quake (Maybe S.Socket)
socket ip port = do
    openedSocket <-
      io $ handle (\(e :: IOException) -> return (Left e)) $ do
        newsocket <- S.socket
        when (isNothing newsocket) $
          ioError (userError "socket creation failed")
        return $ Right (fromJust newsocket)

    case openedSocket of
      Left e -> do
        Com.println $ "Error: " `B.append` (BC.pack $ show e)
        return Nothing
      Right s -> do
        bindResult <- io $ handle (\(e :: IOException) -> return (Left e)) $ do
          if B.length ip == 0 || ip == "localhost"
            then if port == Constants.portAny
                   then void $ S.bind s NS.iNADDR_ANY 0 -- TODO: actually check bind result code
                   else void $ S.bind s NS.iNADDR_ANY port -- TODO: actually check bind result code
            else do
              resolved <- NBSD.getHostByName (BC.unpack ip)
              void $ S.bind s (head $ NBSD.hostAddresses resolved) port -- this head is kinda bad, isn't it? TODO: actually check bind result code
          return $ Right () 

        case bindResult of
          Left e -> do
            io $ S.close s
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
adrToString adr =
    let Just hostAddress = adr^.naIP
        d = (hostAddress `shiftR` 24) .&. 0xFF
        c = (hostAddress `shiftR` 16) .&. 0xFF
        b = (hostAddress `shiftR` 8) .&. 0xFF
        a = hostAddress .&. 0xFF
    in BC.pack $ show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d ++ ":" ++ show (adr^.naPort) -- IMPROVE

-- Gets a packet from a network channel
getPacket :: Int -> Lens' QuakeState NetAdrT -> Lens' QuakeState SizeBufT -> Quake Bool
getPacket sock netFromLens netMessageLens = do
    done <- if sock == Constants.nsClient
              then getLoopPacket (netGlobals.ngLoopbackClient) netFromLens netMessageLens
              else getLoopPacket (netGlobals.ngLoopbackServer) netFromLens netMessageLens

    if sock == Constants.nsClient
      then getNetworkPacket done (netGlobals.ngIpSocketClient)
      else getNetworkPacket done (netGlobals.ngIpSocketServer)

  where getNetworkPacket :: Bool -> Lens' QuakeState (Maybe S.Socket) -> Quake Bool
        getNetworkPacket done socketLens = do
          s <- use socketLens
          if | done -> return True
             | isNothing s -> return False
             | otherwise -> do
                 -- IMPROVE: catch exception?
                 packet <- io $ S.recvFrom (fromJust s) 2048

                 if isJust packet
                   then do
                     let Just (buf, host, port) = packet

                     netFromLens.naIP .= Just host
                     netFromLens.naPort .= fromIntegral port
                     netFromLens.naType .= Constants.naIp

                     let packetLen = B.length buf
                     netMsg <- use netMessageLens

                     if packetLen > (netMsg^.sbMaxSize)
                       then do
                         adr <- use $ netFromLens
                         Com.printf $ "Oversize packet from " `B.append` adrToString adr `B.append` "\n"
                         return False
                       else do
                         -- set the size
                         netMessageLens.sbCurSize .= packetLen
                         -- set data
                         netMessageLens.sbData .= buf
                         return True

                   else return False

-- Compares ip address without the port
compareBaseAdr :: NetAdrT -> NetAdrT -> Bool
compareBaseAdr a b =
    if | (a^.naType) /= (b^.naType) -> False
       | (a^.naType) == Constants.naLoopback -> True
       | (a^.naType) == Constants.naIp -> (a^.naIP) == (b^.naIP) -- TODO: verify it works?
       | otherwise -> False

-- compares ip address and port
compareAdr :: NetAdrT -> NetAdrT -> Bool
compareAdr a b = (a^.naIP) == (b^.naIP) && (a^.naPort) == (b^.naPort)

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
      io (putStrLn $ "sleeping " ++ show msec ++ " msec") >> io (threadDelay $ msec * 1000)

-- Sends a Packet
sendPacket :: Int -> Int -> B.ByteString -> NetAdrT -> Quake ()
sendPacket sock len buf adr = do
    if (adr^.naType) == Constants.naLoopback
      then do
        io $ print "sendPacket#sendLoopPacket"
        io $ print buf
        io $ print sock
        sendLoopPacket sock len buf
      else if sock == Constants.nsServer
             then sendPacket' (netGlobals.ngIpSocketServer)
             else sendPacket' (netGlobals.ngIpSocketClient)

  where sendPacket' :: Lens' QuakeState (Maybe S.Socket) -> Quake ()
        sendPacket' socketLens = do
          s <- use socketLens

          if | isNothing s -> return ()
             | (adr^.naType) /= Constants.naBroadcast && (adr^.naType) /= Constants.naIp ->
                 Com.comError Constants.errFatal "NET_SendPacket: bad address type"
             | otherwise -> do
                 let Just ss = s

                 -- TODO: check all data has been sent
                 void $ io $ S.sendTo ss buf (fromJust $ adr^.naIP) (adr^.naPort)

-- Sends a packet via internal loopback.
sendLoopPacket :: Int -> Int -> B.ByteString -> Quake ()
sendLoopPacket sock len buf = do
    if sock == Constants.nsServer
      then sendLoopPacket' (netGlobals.ngLoopbackClient)
      else sendLoopPacket' (netGlobals.ngLoopbackServer)

  where sendLoopPacket' :: Lens' QuakeState LoopbackT -> Quake ()
        sendLoopPacket' socketLens = do
          loop <- use socketLens
          -- modulo 4
          let i = (loop^.lSend) .&. (maxLoopback - 1)
          socketLens.lSend += 1

          zoom (socketLens.lMsgs.ix i) $ do
            lmData .= buf
            lmDataLen .= len

-- Seems to return true, if the address is on 127.0.0.1
isLocalAddress :: NetAdrT -> Bool
isLocalAddress adr = compareAdr adr netLocalAdr
