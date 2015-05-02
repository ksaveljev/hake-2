module Client.CLView where

import Control.Lens ((^.), use, preuse, ix)
import Control.Monad (liftM, unless)
import Data.Bits ((.&.))

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified Client.SCR as SCR

prepRefresh :: Quake ()
prepRefresh = io (putStrLn "CLView.prepRefresh") >> undefined -- TODO

addNetGraph :: Quake ()
addNetGraph = do
    -- if using the debuggraph for something else, don't
    -- add the net lines
    debugGraphValue <- liftM (^.cvValue) scrDebugGraphCVar
    timeGraphValue <- liftM (^.cvValue) scrTimeGraphCVar

    unless (debugGraphValue == 0 || timeGraphValue == 0) $ do
      dropped <- use $ globals.cls.csNetChan.ncDropped
      mapM_ (\_ -> SCR.debugGraph 30 0x40) [0..dropped-1]

      surpressCount <- use $ globals.cl.csSurpressCount
      mapM_ (\_ -> SCR.debugGraph 30 0xDF) [0..surpressCount-1]

      -- see what the latency was on this packet
      inAck <- use $ globals.cls.csNetChan.ncIncomingAcknowledged
      let idx = inAck .&. (Constants.cmdBackup - 1)

      realTime <- use $ globals.cls.csRealTime
      Just time <- preuse $ globals.cl.csCmdTime.ix idx
      let ping = (realTime - time) `div` 30
          ping' = if ping > 30 then 30 else ping

      SCR.debugGraph (fromIntegral ping') 0xD0
