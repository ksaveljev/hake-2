{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module QCommon.NetChannel where

import Control.Lens (Traversal')
import Control.Monad (void)
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.CVar as CVar
import qualified Sys.Timer as Timer

init :: Quake ()
init = do
    msec <- Timer.milliseconds
    
    let port = msec .&. 0xFFFF

    void $ CVar.get "showpackets" "0" 0
    void $ CVar.get "showdrop" "0" 0
    void $ CVar.get "qport" (BC.pack $ show port) Constants.cvarNoSet

outOfBandPrint :: Int -> NetAdrT -> B.ByteString -> Quake ()
outOfBandPrint = undefined -- TODO

{-
- Netchan_Transmit tries to send an unreliable message to a connection, 
- and handles the transmition / retransmition of the reliable messages.
- 
- A 0 length will still generate a packet and deal with the reliable
- messages.
-}
transmit :: Traversal' QuakeState NetChanT -> Int -> B.ByteString -> Quake ()
transmit _ _ _ = io (putStrLn "NetChannel.transmit") >> undefined -- TODO
