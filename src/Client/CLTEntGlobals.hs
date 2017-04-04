{-# LANGUAGE TemplateHaskell #-}
module Client.CLTEntGlobals where

import           Control.Lens      (makeLenses)
import           Data.IORef        (newIORef)
import qualified Data.Vector       as V
import           System.IO.Unsafe  (unsafePerformIO)

import qualified Constants
import           Client.BeamT
import           Client.CLSustainT
import           Client.ExplosionT
import           Client.LaserT
import           Types

makeLenses ''CLTEntGlobals

initialCLTEntGlobals :: CLTEntGlobals
initialCLTEntGlobals = CLTEntGlobals
    { _clteExplosions         = unsafePerformIO (V.replicateM Constants.maxExplosions (newIORef newExplosionT))
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