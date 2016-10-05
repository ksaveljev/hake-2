{-# LANGUAGE TemplateHaskell #-}
module Client.ClientGlobals
    ( module Client.ClientGlobals
    ) where

import           Control.Lens        (makeLenses)
import qualified Data.ByteString     as B
import qualified Data.Vector         as V
import           Linear              (V3(..))

import           Client.CLightStyleT (newCLightStyleT)
import           Client.KButtonT     (newKButtonT)
import qualified Constants
import           QCommon.SizeBufT    (newSizeBufT)
import           Types

makeLenses ''ClientGlobals

initialClientGlobals :: ClientGlobals
initialClientGlobals = ClientGlobals
    { _cgExtraTime          = 0
    , _cgNumCheatVars       = 0
    , _cgBuf                = newSizeBufT
    , _cgFrameMsec          = 0
    , _cgOldSysFrameTime    = 0
    , _cgInKLook            = newKButtonT
    , _cgInLeft             = newKButtonT
    , _cgInRight            = newKButtonT
    , _cgInForward          = newKButtonT
    , _cgInBack             = newKButtonT
    , _cgInLookUp           = newKButtonT
    , _cgInLookDown         = newKButtonT
    , _cgInMoveLeft         = newKButtonT
    , _cgInMoveRight        = newKButtonT
    , _cgInStrafe           = newKButtonT
    , _cgInSpeed            = newKButtonT
    , _cgInUse              = newKButtonT
    , _cgInAttack           = newKButtonT
    , _cgInUp               = newKButtonT
    , _cgInDown             = newKButtonT
    , _cgInImpulse          = 0
    , _cgLightStyle         = V.replicate Constants.maxLightStyles newCLightStyleT
    , _cgLastOfs            = 0
    , _cgCR                 = 0 -- from Console.hs
    , _cgActiveParticles    = Nothing
    , _cgFreeParticles      = Nothing
    , _cgPrecacheCheck      = 0
    , _cgPrecacheSpawnCount = 0
    , _cgPrecacheTex        = 0
    , _cgPrecacheModelSkin  = 0
    , _cgPrecacheModel      = Nothing
    , _cgNumCLWeaponModels  = 0
    , _cgWeaponModels       = V.replicate Constants.maxClientWeaponModels B.empty
    , _cgPMPassEnt          = Nothing
    , _cgIsDown             = False
    , _cgAVelocities        = V.replicate Constants.numVertexNormals (V3 0 0 0)
    }