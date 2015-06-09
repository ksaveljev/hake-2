{-# LANGUAGE Rank2Types #-}
module Server.SVEnts where

import Control.Lens (use, Lens')
import Control.Monad (when)
import Data.Maybe (isJust)

import Quake
import QuakeState

{-
- Save everything in the world out without deltas. Used for recording
- footage for merged or assembled demos.
-}
recordDemoMessage :: Quake ()
recordDemoMessage = do
    demoFile <- use $ svGlobals.svServerStatic.ssDemoFile

    when (isJust demoFile) $ do
      io (putStrLn "SVEnts.recordDemoMessage") >> undefined -- TODO

writeFrameToClient :: ClientReference -> Lens' QuakeState SizeBufT -> Quake ()
writeFrameToClient _ _ = do
    io (putStrLn "SVEnts.writeFrameToClient") >> undefined -- TODO

buildClientFrame :: ClientReference -> Quake ()
buildClientFrame _ = do
    io (putStrLn "SVEnts.buildClientFrame") >> undefined -- TODO
