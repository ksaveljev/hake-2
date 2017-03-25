module Sound.S
    ( beginRegistration
    , disableStreaming
    , endRegistration
    , getDriverNames
    , initialize
    , rawSamples
    , registerSound
    , shutdown
    , startLocalSound
    , startSound
    , stopAllSounds
    , update
    ) where

import qualified Data.ByteString as B
import qualified Data.Vector     as V
import           Linear          (V3)
import           System.IO       (Handle)

import           Types

initialize :: Quake ()
initialize = io (putStrLn "S.initialize IMPLEMENT ME!") -- TODO

startLocalSound :: B.ByteString -> Quake ()
startLocalSound _ = io (putStrLn "S.startLocalSound IMPLEMENT ME!") -- TODO

stopAllSounds :: Quake ()
stopAllSounds = io (putStrLn "S.stopAllSound IMPLEMENT ME!") -- TODO

update :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> Quake ()
update _ _ _ _ = io (putStrLn "S.update IMPLEMENT ME!") -- TODO

disableStreaming :: Quake ()
disableStreaming = io (putStrLn "S.disableStreaming IMPLEMENT ME!") -- TODO

shutdown :: Quake ()
shutdown = io (putStrLn "S.shutdown IMPLEMENT ME!") -- TODO

registerSound :: B.ByteString -> Quake (Maybe (Ref SfxT))
registerSound _ = io (putStrLn "S.registerSound IMPLEMENT ME!") >> return Nothing -- TODO

rawSamples :: Int -> Int -> Int -> Int -> Handle -> Quake ()
rawSamples _ _ _ _ _ = io (putStrLn "S.rawSamples IMPLEMENT ME!") -- TODO

beginRegistration :: Quake ()
beginRegistration = io (putStrLn "S.beginRegistration IMPLEMENT ME!") -- TODO

endRegistration :: Quake ()
endRegistration = io (putStrLn "S.endRegistration IMPLEMENT ME!") -- TODO

getDriverNames :: Quake (V.Vector B.ByteString)
getDriverNames = error "S.getDriverNames" -- TODO

startSound :: Maybe (V3 Float) -> Ref EdictT -> Int -> Maybe (Ref SfxT) -> Float -> Float -> Float -> Quake ()
startSound _ _ _ _ _ _ _ = io (putStrLn "S.startSound IMPLEMENT ME!") -- TODO