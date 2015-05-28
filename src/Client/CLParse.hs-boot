{-# LANGUAGE Rank2Types #-}
module Client.CLParse where

import Control.Lens (Traversal')
import qualified Data.ByteString as B

import Quake
import QuakeState
import QCommon.XCommandT

downloadF :: XCommandT

parseServerMessage :: Quake ()

checkOrDownloadFile :: B.ByteString -> Quake Bool

registerSounds :: Quake ()

parseClientInfo :: Int -> Quake ()

loadClientInfo :: Traversal' QuakeState ClientInfoT -> B.ByteString -> Quake ()

parseBaseline :: Quake ()
