{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sys.NET
  ( adrToString
  , config
  , initialize
  , sleep
  , stringToAdr
  ) where

import qualified Constants
import qualified QCommon.Com as Com
import           QCommon.NetAdrT
import           QuakeState
import qualified Sys.Socket as Socket
import           Types
import           Util.Binary (encode)
import qualified Util.Lib as Lib

import           Control.Exception (IOException, handle)
import           Control.Lens (use, (^.), (.=), (&), (.~))
import           Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (toLower)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word32)
import qualified Network.BSD as NBSD

netLocalAdr :: NetAdrT
netLocalAdr = newNetAdrT

initialize :: Quake ()
initialize = return () -- nothing to do

config :: Bool -> Quake ()
config multiplayer
  | multiplayer = openIP
  | otherwise =
      do checkIPSocketClient =<< use (netGlobals.ngIpSocketClient)
         checkIPSocketServer =<< use (netGlobals.ngIpSocketServer)
  where checkIPSocketClient Nothing = return ()
        checkIPSocketClient (Just ipSocketClient) =
          do request (io (Socket.close ipSocketClient)) -- IMPROVE: catch exceptions
             netGlobals.ngIpSocketClient .= Nothing
        checkIPSocketServer Nothing = return ()
        checkIPSocketServer (Just ipSocketServer) =
          do request (io (Socket.close ipSocketServer)) -- IMPROVE: catch exceptions
             netGlobals.ngIpSocketServer .= Nothing

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
stringToAdr str
  | strLow `elem` ["localhost", "loopback"] = return (Just netLocalAdr)
  | otherwise =
      do parsedAddress <- request (io (parseAddress (head address)))
         either addressError applyIpAndPort parsedAddress
  where strLow = BC.map toLower str
        address = BC.split ':' str
        addressError e =
          do Com.printf (encode e `B.append` "\n")
             return Nothing
        applyIpAndPort ip
          | length address == 2 =
              let port = Lib.atoi (last address)
              in return (Just (newNetAdrT & naIP .~ Just ip & naType .~ Constants.naIp & naPort .~ port))
          | otherwise = return (Just (newNetAdrT & naIP .~ Just ip & naType .~ Constants.naIp))

parseAddress :: B.ByteString -> IO (Either IOException Word32)
parseAddress hostname =
  handle (\(e :: IOException) -> return (Left e)) $
    do resolved <- NBSD.getHostByName (BC.unpack hostname)
       return (Right (head (NBSD.hostAddresses resolved))) -- IMPROVE: bad head, isn't it ?

openIP :: Quake ()
openIP = error "NET.openIP" -- TODO
