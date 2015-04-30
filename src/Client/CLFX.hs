{-# LANGUAGE MultiWayIf #-}
module Client.CLFX where

-- Client Graphics Effects

import Control.Lens (preuse, (.=), ix, use, (^.))

import Quake
import QuakeState
import qualified Constants

runDLights :: Quake ()
runDLights = do
    time <- use $ globals.cl.csTime
    runDLight (fromIntegral time) 0 Constants.maxDLights

  where runDLight :: Float -> Int -> Int -> Quake ()
        runDLight time idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              Just dl <- preuse $ clientGlobals.cgDLights.ix idx
              if | dl^.cdlRadius == 0 -> runDLight time (idx + 1) maxIdx
                 | dl^.cdlDie < time -> clientGlobals.cgDLights.ix idx.cdlRadius .= 0
                 | otherwise -> runDLight time (idx + 1) maxIdx
                 -- TODO: original quake2 code does have something else
                 -- here (jake2 is missing a part of this function)

runLightStyles :: Quake ()
runLightStyles = io (putStrLn "CLFX.runLightStyles") >> undefined -- TODO
