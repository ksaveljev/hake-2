module QCommon.Com
  (initializeArgv
  ,printf)
  where

import qualified Constants
import           QuakeState
import           Types

import           Control.Lens ((.=))
import           Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

initializeArgv :: [String] -> Quake ()
initializeArgv args =
  do when (len > Constants.maxNumArgvs)
       (comError Constants.errFatal "argc > MAX_NUM_ARGVS")
     comGlobals.cgComArgc .= len
     comGlobals.cgComArgv .= V.fromList (fmap (BC.pack . stripLongArg) args)
  where len = length args 
        stripLongArg s = if length s > Constants.maxTokenChars then "" else s 

printf :: B.ByteString -> Quake ()
printf = error "Com.printf" -- TODO

comError :: Int -> B.ByteString -> Quake ()
comError = error "Com.comError" -- TODO