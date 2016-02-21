module QCommon.NetChannel
  ( initialize
  ) where

import qualified Constants
import qualified QCommon.CVar as CVar
import qualified Sys.Timer as Timer
import           Types
import           Util.Binary (encode)

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
