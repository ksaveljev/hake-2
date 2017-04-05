{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module QCommon.NetChannel where

import Control.Lens (Traversal', Lens', use, (^.), (.=), (+=), preuse, (%=), zoom)
import Control.Monad (void, when, liftM)
import Data.Bits ((.&.), xor, (.|.), complement, shiftL, shiftR)
import Data.Int (Int32)
import Data.Word (Word32)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Game.CVarT
import QCommon.SizeBufT
import Types
import QuakeState
import CVarVariables
import QCommon.NetChanT
import qualified Constants
import {-# SOURCE #-} qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import qualified QCommon.MSG as MSG
import qualified Sys.NET as NET
import qualified QCommon.SZ as SZ
import qualified Sys.Timer as Timer

initialize :: Quake ()
initialize = do
    msec <- Timer.milliseconds
    
    let port = msec .&. 0xFFFF

    void $ CVar.get "showpackets" "0" 0
    void $ CVar.get "showdrop" "0" 0
    void $ CVar.get "qport" (BC.pack $ show port) Constants.cvarNoSet

-- Netchan_OutOfBand. Sends an out-of-band datagram.
outOfBandPrint :: Int -> NetAdrT -> B.ByteString -> Quake ()
outOfBandPrint sock adr buf = do
    -- write the packet header
    SZ.init (netGlobals.ngSend) "" Constants.maxMsgLen
    MSG.writeInt (netGlobals.ngSend) (-1) -- -1 sequence means out of band
    SZ.write (netGlobals.ngSend) buf (B.length buf)

    -- send the datagram
    send <- use $ netGlobals.ngSend
    io $ print "NetChannel.outOfBandPrint NET.sendPacket"
    NET.sendPacket sock (send^.sbCurSize) (send^.sbData) adr

{-
- Netchan_Transmit tries to send an unreliable message to a connection, 
- and handles the transmition / retransmition of the reliable messages.
- 
- A 0 length will still generate a packet and deal with the reliable
- messages.
-}
transmit :: Traversal' QuakeState NetChanT -> Int -> B.ByteString -> Quake ()
transmit netChanLens len buf = do
    Just chan <- preuse netChanLens

    -- check for message overflow
    if chan^.ncMessage.sbOverflowed
      then do
        netChanLens.ncFatalError .= True
        Com.printf $ NET.adrToString (chan^.ncRemoteAddress) `B.append` ":Outgoing message overflow\n"
      else do
        let sendReliable = if needReliable chan then 1 else 0 :: Int

        when ((chan^.ncReliableLength) == 0 && (chan^.ncMessage.sbCurSize) /= 0) $ do
          zoom netChanLens $ do
            ncReliableBuf .= B.take (chan^.ncMessage.sbCurSize) (chan^.ncMessage.sbData) -- TODO: make sure this sbData stuff is correct (jake2 reads from ncMessageBuf)
            ncReliableLength .= (chan^.ncMessage.sbCurSize)
            ncMessage.sbCurSize .= 0
            ncReliableSequence %= (`xor` 1)

        -- write the packet header
        sendBuf <- use $ netChannelGlobals.ncSendBuf
        SZ.init (netChannelGlobals.ncSend) sendBuf Constants.maxMsgLen

        let mask :: Int32 = complement (1 `shiftL` 31)
            w1 = fromIntegral $ (fromIntegral (chan^.ncOutgoingSequence) .&. mask) .|. (fromIntegral sendReliable `shiftL` 31)
            w2 = fromIntegral $ (fromIntegral (chan^.ncIncomingSequence) .&. mask) .|. (fromIntegral (chan^.ncIncomingReliableSequence) `shiftL` 31)

        netChanLens.ncOutgoingSequence += 1
        curTime <- Timer.getCurTime
        netChanLens.ncLastSent .= curTime

        MSG.writeInt (netChannelGlobals.ncSend) w1
        MSG.writeInt (netChannelGlobals.ncSend) w2

        -- send the qport if we are a client
        when ((chan^.ncSock) == Constants.nsClient) $ do
          qport <- liftM (truncate . (^.cvValue)) qportCVar
          -- io (print $ "sending qport = " ++ show qport)
          MSG.writeShort (netChannelGlobals.ncSend) qport

        Just chan' <- preuse netChanLens

        -- copy the reliable message to the packet first
        when (sendReliable /= 0) $ do
          SZ.write (netChannelGlobals.ncSend) (chan'^.ncReliableBuf) (chan'^.ncReliableLength)
          netChanLens.ncLastReliableSequence .= (chan'^.ncOutgoingSequence)

        -- add the unreliable part if space if available
        use (netChannelGlobals.ncSend) >>= \send ->
          if (send^.sbMaxSize) - (send^.sbCurSize) >= len
            then SZ.write (netChannelGlobals.ncSend) buf len
            else Com.printf "Netchan_Transmit: dumped unreliable\n"

        -- send the datagram
        use (netChannelGlobals.ncSend) >>= \send -> do
          io $ print "NetChannel.transmit NET.sendPacket"
          NET.sendPacket (chan'^.ncSock) (send^.sbCurSize) (send^.sbData) (chan'^.ncRemoteAddress)

        showPacketsValue <- liftM (^.cvValue) showPacketsCVar
        when (showPacketsValue /= 0) $ do
          use (netChannelGlobals.ncSend) >>= \send ->
            if sendReliable /= 0
              then
                Com.printf $ "send " `B.append` (BC.pack $ show (send^.sbCurSize)) `B.append`
                             " : s=" `B.append` (BC.pack $ show ((chan'^.ncOutgoingSequence) - 1)) `B.append`
                            " reliable=" `B.append` (BC.pack $ show (chan'^.ncReliableSequence)) `B.append`
                            " ack=" `B.append` (BC.pack $ show (chan'^.ncIncomingSequence)) `B.append`
                            " rack=" `B.append` (BC.pack $ show (chan^.ncIncomingReliableSequence)) `B.append`
                            " data=" `B.append` (send^.sbData) `B.append`
                            "\n"
              else
                Com.printf $ "send " `B.append` (BC.pack $ show (send^.sbCurSize)) `B.append`
                             " : s=" `B.append` (BC.pack $ show ((chan'^.ncOutgoingSequence) - 1)) `B.append`
                            " ack=" `B.append` (BC.pack $ show (chan'^.ncIncomingSequence)) `B.append`
                            " rack=" `B.append` (BC.pack $ show (chan^.ncIncomingReliableSequence)) `B.append`
                            "\n"

{-
- Netchan_Process is called when the current net_message is from remote_address modifies
- net_message so that it points to the packet payload.
-}
process :: Traversal' QuakeState NetChanT -> Lens' QuakeState SizeBufT -> Quake Bool
process netChanLens msgLens = do
    -- get sequence numbers
    MSG.beginReading msgLens
    seqn <- MSG.readLong msgLens
    seqnAck <- MSG.readLong msgLens

    -- read the qport if we are a server
    preuse (netChanLens.ncSock) >>= \(Just sock) ->
      when (sock == Constants.nsServer) $
        void $ MSG.readShort msgLens

    -- TODO: make sure it works! some wierd stuff with unsigned int and shiftR
    let seqnU :: Word32 = fromIntegral seqn
        seqnAckU :: Word32 = fromIntegral seqnAck
        reliableMessage :: Int = fromIntegral (seqnU `shiftR` 31)
        reliableAck :: Int = fromIntegral (seqnAckU `shiftR` 31)
        mask :: Int32 = complement (1 `shiftL` 31)
        seqn' :: Int = fromIntegral (fromIntegral seqn .&. mask)
        seqnAck' :: Int = fromIntegral (fromIntegral seqnAck .&. mask)

    showPacketsValue <- liftM (^.cvValue) showPacketsCVar
    when (showPacketsValue /= 0) $ do
      msg <- use msgLens
      Just chan <- preuse netChanLens
      if reliableMessage /= 0
        then Com.printf $ "recv " `B.append` (BC.pack $ show (msg^.sbCurSize)) `B.append`
                          " : s=" `B.append` (BC.pack $ show seqn') `B.append`
                          " reliable=" `B.append` (BC.pack $ show ((chan^.ncIncomingReliableSequence) `xor` 1)) `B.append`
                          " ack=" `B.append` (BC.pack $ show seqnAck') `B.append`
                          " rack=" `B.append` (BC.pack $ show reliableAck) `B.append`
                          "\n"
        else Com.printf $ "recv " `B.append` (BC.pack $ show (msg^.sbCurSize)) `B.append`
                          " : s=" `B.append` (BC.pack $ show seqn') `B.append`
                          " ack=" `B.append` (BC.pack $ show seqnAck') `B.append`
                          " rack=" `B.append` (BC.pack $ show reliableAck) `B.append`
                          "\n"

    -- discard stale or duplicated packets
    Just chan <- preuse netChanLens

    if seqn' <= (chan^.ncIncomingSequence)
      then do
        showDropValue <- liftM (^.cvValue) showDropCVar
        when (showDropValue /= 0) $
          io (putStrLn "NetChannel.process") >> undefined -- TODO
        return False
      else do
        -- dropped packets don't keep the message from being used 
        let dropped = seqn' - ((chan^.ncIncomingSequence) + 1)
        netChanLens.ncDropped .= dropped
        when (dropped > 0) $ do
          showDropValue <- liftM (^.cvValue) showDropCVar
          when (showDropValue /= 0) $
            io (putStrLn "NetChannel.process") >> undefined -- TODO

        -- if the current outgoing realiable message has been acknowledged
        -- clear the buffer to make way for the next
        when (reliableAck == (chan^.ncReliableSequence)) $
          netChanLens.ncReliableLength .= 0 -- it has been received

        -- if this message contains a reliable message, bump
        -- incoming_reliable_sequence
        zoom netChanLens $ do
          ncIncomingSequence .= seqn'
          ncIncomingAcknowledged .= seqnAck'
          ncIncomingReliableAcknowledged .= reliableAck

        when (reliableMessage /= 0) $
          netChanLens.ncIncomingReliableSequence %= (`xor` 1)

        -- the message can now be read from the current message pointer
        curTime <- Timer.getCurTime
        netChanLens.ncLastReceived .= curTime

        return True

-- Netchan_Setup is called to open a channel to a remote system.
setup :: Int -> Traversal' QuakeState NetChanT -> NetAdrT -> Int -> Quake ()
setup sock netChanLens adr qport = do
    curTime <- Timer.getCurTime

    netChanLens .= newNetChanT { _ncSock = sock
                               , _ncRemoteAddress = adr
                               , _ncRemoteQPort = qport
                               , _ncLastReceived = curTime
                               , _ncIncomingSequence = 0
                               , _ncOutgoingSequence = 1
                               }

    SZ.init (netChanLens.ncMessage) "" (Constants.maxMsgLen - 16)

    netChanLens.ncMessage.sbAllowOverflow .= True

needReliable :: NetChanT -> Bool
needReliable chan =
         -- if the remote side dropped the last reliable message, resend
    if | (chan^.ncIncomingAcknowledged) > (chan^.ncLastReliableSequence) && (chan^.ncIncomingReliableAcknowledged) /= (chan^.ncReliableSequence) -> True
         -- if the reliable transmit buffer is empty, copy the current message out
       | (chan^.ncReliableLength) == 0 && (chan^.ncMessage.sbCurSize) /= 0 -> True
       | otherwise -> False
