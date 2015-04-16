{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module QCommon.NetChannel where

import Control.Lens (Traversal', Lens', use, (^.))
import Control.Monad (void)
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.CVar as CVar
import qualified QCommon.MSG as MSG
import qualified Sys.NET as NET
import qualified QCommon.SZ as SZ
import qualified Sys.Timer as Timer

init :: Quake ()
init = do
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
    NET.sendPacket sock (send^.sbCurSize) (send^.sbData) adr

{-
- Netchan_Transmit tries to send an unreliable message to a connection, 
- and handles the transmition / retransmition of the reliable messages.
- 
- A 0 length will still generate a packet and deal with the reliable
- messages.
-}
transmit :: Traversal' QuakeState NetChanT -> Int -> B.ByteString -> Quake ()
transmit _ _ _ = io (putStrLn "NetChannel.transmit") >> undefined -- TODO

{-
- Netchan_Process is called when the current net_message is from remote_address modifies
- net_message so that it points to the packet payload.
-}
process :: Traversal' QuakeState NetChanT -> Lens' QuakeState SizeBufT -> Quake Bool
process _ _ = io (putStrLn "NetChannel.process") >> undefined -- TODO
