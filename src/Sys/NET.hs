module Sys.NET
  ( adrToString
  , config
  , initialize
  , sleep
  , stringToAdr
  ) where

import           QCommon.NetAdrT
import           Types
import           Util.Binary (encode)

import           Control.Lens ((^.))
import           Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as B
import           Data.Maybe (fromMaybe)

initialize :: Quake ()
initialize = return () -- nothing to do

config :: Bool -> Quake ()
config = error "NET.config" -- TODO

sleep :: Int -> Quake ()
sleep = error "NET.speep" -- TODO

adrToString :: NetAdrT -> B.ByteString
adrToString adr = B.concat [encode a, ".", encode b, ".", encode c, ".", encode d, ":", encode (adr^.naPort)]
  where hostAddress = fromMaybe 0 (adr^.naIP) -- IMPROVE: is 0 as default ok here?
        d = (hostAddress `shiftR` 24) .&. 0xFF
        c = (hostAddress `shiftR` 16) .&. 0xFF
        b = (hostAddress `shiftR` 8) .&. 0xFF
        a = hostAddress .&. 0xFF

stringToAdr :: B.ByteString -> Quake (Maybe NetAdrT)
stringToAdr = error "NET.stringToAdr" -- TODO