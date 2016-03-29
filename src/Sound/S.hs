module Sound.S
  ( disableStreaming
  , initialize
  , shutdown
  , startLocalSound
  , stopAllSounds
  , update
  ) where

import           Types

import qualified Data.ByteString as B
import           Linear (V3)

initialize :: Quake ()
initialize = request (io (putStrLn "S.initialize IMPLEMENT ME!")) -- TODO

startLocalSound :: B.ByteString -> Quake ()
startLocalSound _ = request (io (putStrLn "S.startLocalSound IMPLEMENT ME!")) -- TODO

stopAllSounds :: Quake ()
stopAllSounds = request (io (putStrLn "S.stopAllSound IMPLEMENT ME!")) -- TODO

update :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake ()
update _ _ _ _ = request (io (putStrLn "S.update IMPLEMENT ME!")) -- TODO

disableStreaming :: Quake ()
disableStreaming = request (io (putStrLn "S.disableStreaming IMPLEMENT ME!")) -- TODO

shutdown :: Quake ()
shutdown = request (io (putStrLn "S.shutdown IMPLEMENT ME!")) -- TODO