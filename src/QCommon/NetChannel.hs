{-# LANGUAGE OverloadedStrings #-}
module QCommon.NetChannel where

import Data.Bits ((.&.))
import Control.Lens ((.=))
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

    Just showPacketsCVar <- CVar.get "showpackets" "0" 0
    netChannelGlobals.ncShowPackets .= showPacketsCVar

    Just showDropCVar <- CVar.get "showdrop" "0" 0
    netChannelGlobals.ncShowDrop .= showDropCVar

    Just qPortCVar <- CVar.get "qport" (BC.pack $ show port) Constants.cvarNoSet -- IMPROVE: convert Int to ByteString using binary package?
    netChannelGlobals.ncQPort .= qPortCVar
