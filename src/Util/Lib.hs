module Util.Lib where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

atof :: B.ByteString -> Float
atof str = if B.length str == 0
             then 0.0
             else read (BC.unpack str) -- TODO: use binary package for conversion?
