module Client.VIDShared
  ( printf
  ) where

import qualified Constants
import qualified QCommon.Com as Com
import           Types

import qualified Data.ByteString as B

printf :: Int -> B.ByteString -> Quake ()
printf printLevel str
  | printLevel == Constants.printAll = Com.printf str
  | otherwise = Com.dprintf str