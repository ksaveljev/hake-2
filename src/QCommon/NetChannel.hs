{-# LANGUAGE Rank2Types #-}
module QCommon.NetChannel
  ( initialize
  , outOfBandPrint
  , process
  ) where

import qualified Constants
import qualified QCommon.CVar as CVar
import qualified Sys.Timer as Timer
import           Types
import           Util.Binary (encode)

import           Control.Lens (Traversal', Lens')
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
outOfBandPrint = error "NetChannel.outOfBandPrint" -- TODO

process :: Traversal' QuakeState NetChanT -> Lens' QuakeState SizeBufT -> Quake Bool
process = error "NetChannel.process" -- TODO