{-# LANGUAGE TemplateHaskell #-}
module Client.CLTEntGlobals
  ( module Client.CLTEntGlobals
  ) where

import           Client.BeamT (newBeamT)
import           Client.CLSustainT (newCLSustainT)
import           Client.ExplosionT (newExplosionT)
import           Client.LaserT (newLaserT)
import qualified Constants
import           Types

import           Control.Lens (makeLenses)
import qualified Data.Vector as V

makeLenses ''CLTEntGlobals

initialCLTEntGlobals :: CLTEntGlobals
initialCLTEntGlobals =
  CLTEntGlobals { _clteExplosions         = V.replicate Constants.maxExplosions newExplosionT
                , _clteBeams              = V.replicate Constants.maxBeams newBeamT
                , _cltePlayerBeams        = V.replicate Constants.maxBeams newBeamT
                , _clteLasers             = V.replicate Constants.maxLasers newLaserT
                , _clteSustains           = V.replicate Constants.maxSustains newCLSustainT
                , _clteSfxRic1            = Nothing
                , _clteSfxRic2            = Nothing
                , _clteSfxRic3            = Nothing
                , _clteSfxLashIt          = Nothing
                , _clteSfxSpark5          = Nothing
                , _clteSfxSpark6          = Nothing
                , _clteSfxSpark7          = Nothing
                , _clteSfxRailg           = Nothing
                , _clteSfxRockExp         = Nothing
                , _clteSfxGrenExp         = Nothing
                , _clteSfxWatrExp         = Nothing
                , _clteSfxPlasExp         = Nothing
                , _clteSfxFootsteps       = V.replicate 4 Nothing
                , _clteModExplode         = Nothing
                , _clteModSmoke           = Nothing
                , _clteModFlash           = Nothing
                , _clteModParasiteSegment = Nothing
                , _clteModGrappleCable    = Nothing
                , _clteModParasiteTip     = Nothing
                , _clteModExplo4          = Nothing
                , _clteModBfgExplo        = Nothing
                , _clteModPowerScreen     = Nothing
                , _clteModPlasmaExplo     = Nothing
                , _clteSfxLightning       = Nothing
                , _clteSfxDisrExp         = Nothing
                , _clteModLightning       = Nothing
                , _clteModHeatBeam        = Nothing
                , _clteModMonsterHeatBeam = Nothing
                , _clteModExplo4Big       = Nothing
                }