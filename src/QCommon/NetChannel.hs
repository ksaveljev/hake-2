{-# LANGUAGE OverloadedStrings #-}
module QCommon.NetChannel where

import Data.Bits ((.&.))
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QuakeState
import QCommon.NetAdrT
import qualified Constants
import qualified QCommon.CVar as CVar
import qualified Sys.Timer as Timer

init :: Quake ()
init = do
    msec <- Timer.milliseconds
    
    let port = msec .&. 0xFFFF

    void $ CVar.getAndSet "showpackets" "0" 0 (netChannelGlobals.ncShowPackets)
    void $ CVar.getAndSet "showdrop" "0" 0 (netChannelGlobals.ncShowDrop)
    void $ CVar.getAndSet "qport" (BC.pack $ show port) Constants.cvarNoSet (netChannelGlobals.ncQPort) -- IMPROVE: convert Int to ByteString using binary package?

outOfBandPrint :: Int -> NetAdrT -> B.ByteString -> Quake ()
outOfBandPrint = undefined -- TODO
