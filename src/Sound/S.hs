module Sound.S where

import Data.IORef (IORef)
import Linear (V3)
import qualified Data.ByteString as B

import Quake
import QuakeState
import Sound.SfxT

init :: Quake ()
init = return () -- TODO: don't want to get involved with sound system yet -- io (putStrLn "S.init") >> undefined -- TODO

shutdown :: Quake ()
shutdown = return () -- TODO: don't want to get involved with sound system yet -- io (putStrLn "S.shutdown") >> undefined -- TODO

stopAllSounds :: Quake ()
stopAllSounds = return () -- TODO: don't want to get involved with sound system yet
    -- io (putStrLn "S.stopAllSounds") >> undefined -- TODO

update :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake ()
update _ _ _ _ = io (putStrLn "S.update IMPLEMENT ME!") >> return () -- io (putStrLn "S.update") >> undefined -- TODO

startLocalSound :: B.ByteString -> Quake ()
startLocalSound _ = do
    io (putStrLn "S.startLocalSound") >> undefined -- TODO

disableStreaming :: Quake ()
disableStreaming = io (putStrLn "S.disableStreaming") >> undefined -- TODO

registerSound :: B.ByteString -> Quake (Maybe (IORef SfxT))
registerSound _ = do
    io (putStrLn "S.registerSound") >> undefined -- TODO

startSound :: Maybe (V3 Float) -> EdictReference -> Int -> Maybe (IORef SfxT) -> Float -> Float -> Float -> Quake ()
startSound _ _ _ _ _ _ _ = do
    return () -- TODO: don't want to get involved with sound system yet
    --io (putStrLn "S.startSound") >> undefined -- TODO
