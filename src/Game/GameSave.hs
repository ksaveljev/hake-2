module Game.GameSave where

import qualified Data.ByteString as B

import Quake

writeLevel :: B.ByteString -> Quake ()
writeLevel _ = io (putStrLn "GameSave.writeLevel") >> undefined -- TODO

readLevel :: B.ByteString -> Quake ()
readLevel _ = io (putStrLn "GameSave.readLevel") >> undefined -- TODO

initGame :: Quake ()
initGame = io (putStrLn "GameSave.initGame") >> undefined -- TODO
