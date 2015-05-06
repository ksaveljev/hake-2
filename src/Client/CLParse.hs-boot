module Client.CLParse where

import qualified Data.ByteString as B

import Quake
import QCommon.XCommandT

downloadF :: XCommandT

parseServerMessage :: Quake ()

checkOrDownloadFile :: B.ByteString -> Quake Bool

registerSounds :: Quake ()
