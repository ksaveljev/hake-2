module Server.SVEnts where

import Quake

{-
- Save everything in the world out without deltas. Used for recording
- footage for merged or assembled demos.
-}
recordDemoMessage :: Quake ()
recordDemoMessage = io (putStrLn "SVEnts.recordDemoMessage") >> undefined -- TODO
