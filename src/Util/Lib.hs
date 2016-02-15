module Util.Lib where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe (fromMaybe)
import           Text.Read (readMaybe)

atof :: B.ByteString -> Float
atof str = if B.length str == 0
             then 0.0
             else fromMaybe 0.0 (readMaybe (BC.unpack str)) -- IMPROVE?