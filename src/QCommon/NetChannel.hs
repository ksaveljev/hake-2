{-# LANGUAGE Rank2Types #-}
module QCommon.NetChannel
  ( initialize
  , outOfBandPrint
  , process
  , setup
  , transmit
  ) where

import qualified Constants
import           Game.CVarT
import qualified QCommon.Com as Com
import qualified QCommon.CVar as CVar
import           QCommon.CVarVariables
import qualified QCommon.MSG as MSG
import           QCommon.NetChanT
import           QCommon.SizeBufT
import qualified QCommon.SZ as SZ
import           QuakeState
import qualified Sys.NET as NET
import qualified Sys.Timer as Timer
import           Types
import           Util.Binary (encode)

import           Control.Lens (Traversal', Lens', preuse, use, (^.), (.=), (+=), (%=), (&), (.~), (%~))
import           Control.Monad (void, when)
import           Data.Bits (complement, shiftL, xor, (.&.), (.|.))
import qualified Data.ByteString as B
import           Data.Int (Int32)

initialize :: Quake ()
initialize =
  do msec <- Timer.milliseconds
     CVar.initializeCVars initialCVars
     void (CVar.get "qport" (encode (msec .&. 0xFFFF)) Constants.cvarNoSet)

initialCVars :: [(B.ByteString, B.ByteString, Int)]
initialCVars = [ ("showpackets", "0", 0)
               , ("showdrop", "0", 0)
               ]

outOfBandPrint :: Int -> NetAdrT -> B.ByteString -> Quake ()
outOfBandPrint sock adr buf =
  do SZ.initialize (netGlobals.ngSend) B.empty Constants.maxMsgLen
     MSG.writeInt (netGlobals.ngSend) (-1) -- -1 sequence means out of band
     SZ.write (netGlobals.ngSend) buf (B.length buf)
     send <- use (netGlobals.ngSend)
     NET.sendPacket sock (send^.sbCurSize) (send^.sbData) adr

process :: Traversal' QuakeState NetChanT -> Lens' QuakeState SizeBufT -> Quake Bool
process = error "NetChannel.process" -- TODO

transmit :: Traversal' QuakeState NetChanT -> Int -> B.ByteString -> Quake ()
transmit netChanLens len buf =
  do chan <- preuse netChanLens
     maybe chanError (proceedTransmit netChanLens len buf) chan
  where chanError = Com.fatalError "NetChannel.transmit chan is Nothing"

proceedTransmit :: Traversal' QuakeState NetChanT -> Int -> B.ByteString -> NetChanT -> Quake ()
proceedTransmit netChanLens len buf chan
  | chan^.ncMessage.sbOverflowed =
      do netChanLens.ncFatalError .= True
         Com.printf (NET.adrToString (chan^.ncRemoteAddress) `B.append` ":Outgoing message overflow\n")
  | otherwise =
      do setReliable
         writePacketHeader
         chan' <- preuse netChanLens
         maybe chanError (doTransmit netChanLens len buf sendReliable) chan'
  where sendReliable | needReliable chan = 1 :: Int
                     | otherwise = 0
        setReliable =
          when ((chan^.ncReliableLength) == 0 && (chan^.ncMessage.sbCurSize) /= 0) $
            do netChanLens %= (\v -> v & ncReliableBuf .~ B.take (chan^.ncMessage.sbCurSize) (chan^.ncMessage.sbData) -- TODO: make sure this sbData stuff is correct (jake2 reads from ncMessageBuf)
                                       & ncReliableLength .~ (chan^.ncMessage.sbCurSize)
                                       & ncMessage.sbCurSize .~ 0
                                       & ncReliableSequence %~ (`xor` 1))
        writePacketHeader =
          do SZ.initialize (netChannelGlobals.ncSend) B.empty Constants.maxMsgLen
             netChanLens.ncOutgoingSequence += 1
             curTime <- use (globals.gCurTime)
             netChanLens.ncLastSent .= curTime
             MSG.writeInt (netChannelGlobals.ncSend) w1
             MSG.writeInt (netChannelGlobals.ncSend) w2
             when ((chan^.ncSock) == Constants.nsClient) $
               do qport <- fmap (truncate . (^.cvValue)) qportCVar
                  MSG.writeShort (netChannelGlobals.ncSend) qport
        mask = complement (1 `shiftL` 31) :: Int32
        w1 = fromIntegral ((fromIntegral (chan^.ncOutgoingSequence) .&. mask) .|. (fromIntegral sendReliable `shiftL` 31))
        w2 = fromIntegral ((fromIntegral (chan^.ncIncomingSequence) .&. mask) .|. (fromIntegral (chan^.ncIncomingReliableSequence) `shiftL` 31))
        chanError = Com.fatalError "NetChannel.proceedTransmit chan is Nothing"

doTransmit :: Traversal' QuakeState NetChanT -> Int -> B.ByteString -> Int -> NetChanT -> Quake ()
doTransmit netChanLens len buf sendReliable chan =
  do addReliablePart
     addUnreliablePart =<< use (netChannelGlobals.ncSend)
     sendDatagram =<< use (netChannelGlobals.ncSend)
     showPacketInfo =<< showPacketsCVar
  where addReliablePart =
          when (sendReliable /= 0) $
            do SZ.write (netChannelGlobals.ncSend) (chan^.ncReliableBuf) (chan^.ncReliableLength)
               netChanLens.ncLastReliableSequence .= (chan^.ncOutgoingSequence)
        addUnreliablePart send
          | (send^.sbMaxSize) - (send^.sbCurSize) >= len =
              SZ.write (netChannelGlobals.ncSend) buf len
          | otherwise = Com.printf "Netchan_Transmit: dumped unreliable\n"
        sendDatagram send =
          NET.sendPacket (chan^.ncSock) (send^.sbCurSize) (send^.sbData) (chan^.ncRemoteAddress)
        showPacketInfo showPackets
          | (showPackets^.cvValue) /= 0 =
              error "NetChannel.doTransmit showPacketInfo" -- TODO
{-
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
                            -}
          | otherwise = return ()

setup :: Int -> Traversal' QuakeState NetChanT -> NetAdrT -> Int -> Quake ()
setup sock netChanLens adr qport =
  do curTime <- use (globals.gCurTime)
     netChanLens .= newNetChanT { _ncSock = sock
                                , _ncRemoteAddress = adr
                                , _ncRemoteQPort = qport
                                , _ncLastReceived = curTime
                                , _ncIncomingSequence = 0
                                , _ncOutgoingSequence = 1
                                }
     SZ.initialize (netChanLens.ncMessage) B.empty (Constants.maxMsgLen - 16)
     netChanLens.ncMessage.sbAllowOverflow .= True

needReliable :: NetChanT -> Bool
needReliable chan
  | (chan^.ncIncomingAcknowledged) > (chan^.ncLastReliableSequence) && (chan^.ncIncomingReliableAcknowledged) /= (chan^.ncReliableSequence) = True
  | (chan^.ncReliableLength) == 0 && (chan^.ncMessage.sbCurSize) /= 0 = True
  | otherwise = False