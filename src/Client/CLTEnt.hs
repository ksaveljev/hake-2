module Client.CLTEnt where

import Control.Lens (zoom, (.=))
import qualified Data.Vector as V

import Quake
import QuakeState
import qualified Constants

clearTEnts :: Quake ()
clearTEnts = do
    zoom clTEntGlobals $ do
      clteExplosions  .= V.replicate Constants.maxExplosions newExplosionT
      clteBeams       .= V.replicate Constants.maxBeams newBeamT
      cltePlayerBeams .= V.replicate Constants.maxBeams newBeamT
      clteLasers      .= V.replicate Constants.maxLasers newLaserT
      clteSustains    .= V.replicate Constants.maxSustains newCLSustainT

registerTEntModels :: Quake ()
registerTEntModels = do
    io (putStrLn "CLTEnt.registerTEntModels") >> undefined -- TODO
