module Client.CLTEnt
  ( clearTEnts
  , parseTEnt
  ) where

import           Client.BeamT
import           Client.CLSustainT
import           Client.ExplosionT
import           Client.LaserT
import qualified Constants
import           QuakeState
import           Types

import           Control.Lens ((%=), (&), (.~))
import qualified Data.Vector as V

clearTEnts :: Quake ()
clearTEnts =
  clTEntGlobals %= (\v -> v & clteExplosions .~ V.replicate Constants.maxExplosions newExplosionT
                            & clteBeams .~ V.replicate Constants.maxBeams newBeamT
                            & cltePlayerBeams .~ V.replicate Constants.maxBeams newBeamT
                            & clteLasers .~ V.replicate Constants.maxLasers newLaserT
                            & clteSustains .~ V.replicate Constants.maxSustains newCLSustainT)

parseTEnt :: Quake ()
parseTEnt = error "CLTEnt.parseTEnt" -- TODO