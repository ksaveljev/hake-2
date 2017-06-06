module Client.CLInv
    ( drawInventory
    , parseInventory
    ) where

import           Control.Lens        (ix, (.=))

import           Client.ClientStateT
import qualified Constants
import qualified QCommon.MSG         as MSG
import           QuakeState
import           Types

drawInventory :: Quake ()
drawInventory = error "CLInv.drawInventory" -- TODO

parseInventory :: Quake ()
parseInventory = mapM_ readInventory [0..Constants.maxItems-1]
  where
    readInventory idx = do
        v <- MSG.readShort (globals.gNetMessage)
        globals.gCl.csInventory.ix idx .= v -- IMPROVE: too much alloc here?
