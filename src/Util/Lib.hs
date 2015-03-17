module Util.Lib where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

atof :: B.ByteString -> Float
atof str = if B.length str == 0
             then 0.0
             else fromMaybe 0.0 (readMaybe (BC.unpack str)) -- IMPROVE: use binary package for conversion?

atoi :: B.ByteString -> Int
atoi str = if B.length str == 0
             then 0
             else fromMaybe 0 (readMaybe (BC.unpack str)) -- IMPROVE: use binary package for conversion?
