{-# LANGUAGE Rank2Types #-}
module QCommon.NetChannel
  ( initialize
  , outOfBandPrint
  , process
  , transmit
  ) where

import qualified Constants
import qualified QCommon.CVar as CVar
import qualified QCommon.MSG as MSG
import           QCommon.SizeBufT
import qualified QCommon.SZ as SZ
import           QuakeState
import qualified Sys.NET as NET
import qualified Sys.Timer as Timer
import           Types
import           Util.Binary (encode)

import           Control.Lens (Traversal', Lens', use, (^.))
import           Control.Monad (void)
import           Data.Bits ((.&.))
import qualified Data.ByteString as B

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
transmit = error "NetChannel.transmit" -- TODO