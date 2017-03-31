module Client.CLInv where

import Control.Lens ((.=), ix)

import Types
import QuakeState
import qualified Constants
import qualified QCommon.MSG as MSG

drawInventory :: Quake ()
drawInventory = do
    io (putStrLn "CLInv.drawInventory") >> undefined -- TODO

-- IMPROVE: collect the whole vector and then update the state
parseInventory :: Quake ()
parseInventory =
    readInventory 0 Constants.maxItems

  where readInventory :: Int -> Int -> Quake ()
        readInventory idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              v <- MSG.readShort (globals.gNetMessage)
              globals.gCl.csInventory.ix idx .= v
