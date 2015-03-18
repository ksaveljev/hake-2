{-# LANGUAGE OverloadedStrings #-}
module QCommon.NetChannel where

import Data.Bits ((.&.))
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Quake
import QCommon.NetAdrT
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
