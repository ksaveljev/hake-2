module QCommon.Com
  ( argv
  , clearArgv
  , comError
  , fatalError
  , initializeArgv
  , printf )
  where

import qualified Constants
import           QuakeState
import           Types

import           Control.Lens (use, (.=), (%=))
import           Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V

initializeArgv :: [String] -> Quake ()
initializeArgv args =
  do when (len > Constants.maxNumArgvs) $
       fatalError "argc > MAX_NUM_ARGVS"
     comGlobals.cgComArgc .= len
     comGlobals.cgComArgv .= V.fromList (fmap (BC.pack . stripLongArg) args)
  where len = length args 
        stripLongArg s = if length s > Constants.maxTokenChars then "" else s 

printf :: B.ByteString -> Quake ()
printf = error "Com.printf" -- TODO

comError :: Int -> B.ByteString -> Quake ()
comError = error "Com.comError" -- TODO

fatalError :: B.ByteString -> Quake ()
fatalError = comError Constants.errFatal

argv :: Int -> Quake B.ByteString
argv idx = do
  comArgv <- use (comGlobals.cgComArgv)
  return (fromMaybe "" (comArgv V.!? idx))

clearArgv :: Int -> Quake ()
clearArgv idx =
  comGlobals.cgComArgv %= (\v -> if idx < 0 || idx >= V.length v then v else v V.// [(idx, "")])
