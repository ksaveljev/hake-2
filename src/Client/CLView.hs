module Client.CLView
  ( addNetGraph
  , prepRefresh
  ) where

import           Client.ClientStateT
import           Client.ClientStaticT
import qualified Client.SCR as SCR
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com as Com
import           QCommon.CVarVariables
import           QCommon.NetChanT
import           QuakeState
import           Types

import           Control.Applicative (liftA2)
import           Control.Lens (use, preuse, ix, (^.))
import           Control.Monad (join)
import           Data.Bits ((.&.))

addNetGraph :: Quake ()
addNetGraph = join (liftA2 proceedAddNetGraph scrDebugGraphCVar scrTimeGraphCVar)
  where proceedAddNetGraph debugGraph timeGraph
          | (debugGraph^.cvValue) == 0 || (timeGraph^.cvValue) == 0 = return ()
          | otherwise =
              do dropped <- use (globals.gCls.csNetChan.ncDropped)
                 mapM_ (const (SCR.debugGraph 30 0x40)) [0..dropped-1]
                 surpressCount <- use (globals.gCl.csSurpressCount)
                 mapM_ (const (SCR.debugGraph 30 0xDF)) [0..surpressCount-1]
                 idx <- fmap (.&. (Constants.cmdBackup - 1)) (use (globals.gCls.csNetChan.ncIncomingAcknowledged))
                 realTime <- use (globals.gCls.csRealTime)
                 time <- preuse (globals.gCl.csCmdTime.ix idx)
                 maybe timeError (doAddNetGraph realTime) time
        timeError = Com.fatalError "CLView.addNetGraph time is Nothing"
        doAddNetGraph realTime time =
          SCR.debugGraph (fromIntegral (min ((realTime - time `div` 30)) 30)) 0xD0

prepRefresh :: Quake ()
prepRefresh = error "CLView.prepRefresh" -- TODO