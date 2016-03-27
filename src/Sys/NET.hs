{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Sys.NET
  ( adrToString
  , compareAdr
  , compareBaseAdr
  , config
  , getPacket
  , initialize
  , isLocalAddress
  , sendPacket
  , sleep
  , stringToAdr
  ) where

import qualified Constants
import           Game.CVarT
import qualified QCommon.Com as Com
import           QCommon.CVarVariables
import           QCommon.NetAdrT
import           QCommon.SizeBufT
import           QuakeState
import           Sys.LoopbackT
import           Sys.LoopMsgT
import qualified Sys.Socket as Socket
import           Types
import           Util.Binary (encode)
import qualified Util.Lib as Lib

import           Control.Concurrent (threadDelay)
import           Control.Exception (IOException, handle)
import           Control.Lens (Lens', preuse, use, ix, (^.), (.=), (+=), (%=), (&), (.~))
import           Control.Monad (when, void)
import           Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (toLower)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word32)
import qualified Network.BSD as NBSD

netLocalAdr :: NetAdrT
netLocalAdr = newNetAdrT

maxLoopback :: Int
maxLoopback = 4

initialize :: Quake ()
initialize = return () -- nothing to do

config :: Bool -> Quake ()
config multiplayer
  | multiplayer = openIP
  | otherwise =
      do checkIPSocketClient =<< use (netGlobals.ngIpSocketClient)
         checkIPSocketServer =<< use (netGlobals.ngIpSocketServer)
  where checkIPSocketClient Nothing = return ()
        checkIPSocketClient (Just ipSocketClient) =
          do request (io (Socket.close ipSocketClient)) -- IMPROVE: catch exceptions
             netGlobals.ngIpSocketClient .= Nothing
        checkIPSocketServer Nothing = return ()
        checkIPSocketServer (Just ipSocketServer) =
          do request (io (Socket.close ipSocketServer)) -- IMPROVE: catch exceptions
             netGlobals.ngIpSocketServer .= Nothing

sleep :: Int -> Quake ()
sleep msec =
  do ipSocketServer <- use (netGlobals.ngIpSocketServer)
     dedicated <- dedicatedCVar
     proceedSleep ipSocketServer dedicated
  where proceedSleep Nothing _ = return ()
        proceedSleep (Just _) dedicated
          | (dedicated^.cvValue) == 0 = return ()
          | otherwise = request (io netSleep)
        netSleep =
          do print (B.concat ["sleeping ", encode msec, " msec"])
             threadDelay (msec * 1000)

adrToString :: NetAdrT -> B.ByteString
adrToString adr = B.concat [encode a, ".", encode b, ".", encode c, ".", encode d, ":", encode (adr^.naPort)]
  where hostAddress = fromMaybe 0 (adr^.naIP) -- IMPROVE: is 0 as default ok here?
        d = (hostAddress `shiftR` 24) .&. 0xFF
        c = (hostAddress `shiftR` 16) .&. 0xFF
        b = (hostAddress `shiftR` 8) .&. 0xFF
        a = hostAddress .&. 0xFF

stringToAdr :: B.ByteString -> Quake (Maybe NetAdrT)
stringToAdr str
  | strLow `elem` ["localhost", "loopback"] = return (Just netLocalAdr)
  | otherwise =
      do parsedAddress <- request (io (parseAddress (head address)))
         either addressError applyIpAndPort parsedAddress
  where strLow = BC.map toLower str
        address = BC.split ':' str
        addressError e =
          do Com.printf (encode e `B.append` "\n")
             return Nothing
        applyIpAndPort ip
          | length address == 2 =
              let port = Lib.atoi (last address)
              in return (Just (newNetAdrT & naIP .~ Just ip & naType .~ Constants.naIp & naPort .~ port))
          | otherwise = return (Just (newNetAdrT & naIP .~ Just ip & naType .~ Constants.naIp))

parseAddress :: B.ByteString -> IO (Either IOException Word32)
parseAddress hostname =
  handle (\(e :: IOException) -> return (Left e)) $
    do resolved <- NBSD.getHostByName (BC.unpack hostname)
       return (Right (head (NBSD.hostAddresses resolved))) -- IMPROVE: bad head, isn't it ?

openIP :: Quake ()
openIP = error "NET.openIP" -- TODO

getPacket :: Int -> Lens' QuakeState NetAdrT -> Lens' QuakeState SizeBufT -> Quake Bool
getPacket sock netFromLens netMessageLens =
  do done <- getLoopPacket fromLoopbackLens netFromLens netMessageLens
     getNetworkPacket netFromLens netMessageLens done fromIpSocketLens
  where fromLoopbackLens :: Lens' QuakeState LoopbackT
        fromLoopbackLens
          | sock == Constants.nsClient = netGlobals.ngLoopbackClient
          | otherwise = netGlobals.ngLoopbackServer
        fromIpSocketLens :: Lens' QuakeState (Maybe Socket)
        fromIpSocketLens
          | sock == Constants.nsClient = netGlobals.ngIpSocketClient
          | otherwise = netGlobals.ngIpSocketServer

getLoopPacket :: Lens' QuakeState LoopbackT -> Lens' QuakeState NetAdrT -> Lens' QuakeState SizeBufT -> Quake Bool
getLoopPacket loopbackLens netFromLens netMessageLens =
  do checkLoopbackSendGet loopbackLens
     checkPacketContents =<< use loopbackLens
  where checkPacketContents loop
          | (loop^.lGet) >= (loop^.lSend) = return False
          | otherwise =
              do loopbackLens.lGet += 1
                 msg <- preuse (loopbackLens.lMsgs.ix ((loop^.lGet) .&. (maxLoopback - 1)))
                 maybe msgError readMsg msg
        msgError = error "NET.getLoopPacket netLoopMsgT is Nothing"
        readMsg msg =
          do netMessageLens.sbData .= B.take (msg^.lmDataLen) (msg^.lmData)
             netMessageLens.sbCurSize .= (msg^.lmDataLen)
             netFromLens .= netLocalAdr
             return True

checkLoopbackSendGet :: Lens' QuakeState LoopbackT -> Quake ()
checkLoopbackSendGet loopbackLens =
  do loop <- use loopbackLens
     when ((loop^.lSend) - (loop^.lGet) > maxLoopback) $
       loopbackLens.lGet .= (loop^.lSend) - maxLoopback

getNetworkPacket :: Lens' QuakeState NetAdrT -> Lens' QuakeState SizeBufT -> Bool -> Lens' QuakeState (Maybe Socket) -> Quake Bool
getNetworkPacket _ _ True _ = return True
getNetworkPacket netFromLens netMessageLens False socketLens =
  do socket <- use socketLens
     maybe (return False) readNetworkPacket socket
  where readNetworkPacket socket =
          do packet <- request (io (Socket.recvFrom socket 2048))
             maybe (return False) networkPacket packet
        networkPacket (buf, host, port) =
          do netFromLens.naIP .= Just host
             netFromLens.naPort .= fromIntegral port
             netFromLens.naType .= Constants.naIp
             checkBuffer buf =<< use netMessageLens
        checkBuffer buf netMsg
          | B.length buf > (netMsg^.sbMaxSize) =
              do adr <- use netFromLens
                 Com.printf (B.concat ["Oversize packet from ", adrToString adr, "\n"])
                 return False
          | otherwise =
              do netMessageLens.sbCurSize .= B.length buf
                 netMessageLens.sbData .= buf
                 return True

compareBaseAdr :: NetAdrT -> NetAdrT -> Bool
compareBaseAdr a b
  | (a^.naType) /= (b^.naType) = False
  | (a^.naType) == Constants.naLoopback = True
  | (a^.naType) == Constants.naIp = (a^.naIP) == (b^.naIP) -- TODO: verify it works?
  | otherwise = False

compareAdr :: NetAdrT -> NetAdrT -> Bool
compareAdr a b = (a^.naIP) == (b^.naIP) && (a^.naPort) == (b^.naPort)

sendPacket :: Int -> Int -> B.ByteString -> NetAdrT -> Quake ()
sendPacket sock len buf adr
  | (adr^.naType) == Constants.naLoopback = sendLoopPacket sock len buf
  | sock == Constants.nsServer = doSendPacket buf adr =<< use (netGlobals.ngIpSocketServer)
  | otherwise = doSendPacket buf adr =<< use (netGlobals.ngIpSocketClient)

doSendPacket :: B.ByteString -> NetAdrT -> Maybe Socket -> Quake ()
doSendPacket _ _ Nothing = return ()
doSendPacket buf adr (Just sock)
  | (adr^.naType) `notElem` [Constants.naBroadcast, Constants.naIp] =
      Com.fatalError "NET_SendPacket: bad address type"
  | otherwise = maybe addressIPError doSend (adr^.naIP)
  where addressIPError = error "NET.doSendPacket adr^.naIP is Nothing"
        doSend ip = void (request (io (Socket.sendTo sock buf ip (adr^.naPort))))

sendLoopPacket :: Int -> Int -> B.ByteString -> Quake ()
sendLoopPacket sock len buf
  | sock == Constants.nsServer = doSendLoopPacket buf len (netGlobals.ngLoopbackClient)
  | otherwise = doSendLoopPacket buf len (netGlobals.ngLoopbackServer)

doSendLoopPacket :: B.ByteString -> Int -> Lens' QuakeState LoopbackT -> Quake ()
doSendLoopPacket buf len socketLens =
  do loop <- use socketLens
     socketLens.lSend += 1
     setMsgData ((loop^.lSend) .&. (maxLoopback - 1))
  where setMsgData idx =
          socketLens.lMsgs.ix idx %= (\v -> v & lmData .~ buf
                                              & lmDataLen .~ len)

isLocalAddress :: NetAdrT -> Bool
isLocalAddress adr = compareAdr adr netLocalAdr
