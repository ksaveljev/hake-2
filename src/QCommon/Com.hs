module QCommon.Com
  (initializeArgv
  ,printf)
  where

import           Types

import qualified Data.ByteString as B

initializeArgv :: [String] -> Quake ()
initializeArgv = undefined -- TODO

printf :: B.ByteString -> Quake ()
printf = undefined -- TODO
