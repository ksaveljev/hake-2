{-# LANGUAGE Rank2Types #-}
module Client.CLParse where

import Control.Lens (Traversal')
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Types
import QuakeState
import QCommon.XCommandT

svcStrings :: V.Vector B.ByteString

downloadF :: XCommandT

showNet :: B.ByteString -> Quake ()

parseServerMessage :: Quake ()

checkOrDownloadFile :: B.ByteString -> Quake Bool

registerSounds :: Quake ()

parseClientInfo :: Int -> Quake ()

loadClientInfo :: Traversal' QuakeState ClientInfoT -> B.ByteString -> Quake ()

parseBaseline :: Quake ()
