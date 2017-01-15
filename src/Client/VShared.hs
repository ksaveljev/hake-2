module Client.VShared
    ( addEntity
    , addLight
    , addLightStyle
    , addParticle
    ) where

import           Control.Lens                 (use, ix, (^.), (.=), (+=))
import           Control.Monad                (when, unless)
import           Data.Bits                    (shiftL, (.|.))
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed          as UV
import           Linear                       (V3(..), _x, _y, _z)

import qualified Constants
import           QuakeIOState
import           QuakeRef
import           QuakeState
import           Types

addEntity :: EntityT -> Quake ()
addEntity entity = do
    numEntities <- use (vGlobals.vgNumEntities)
    when (numEntities < Constants.maxEntities) $ do
      vGlobals.vgEntities.ix numEntities .= entity
      vGlobals.vgNumEntities += 1

addLight :: V3 Float -> Float -> Float -> Float -> Float -> Quake ()
addLight org intensity r g b = do
    numDLights <- use (vGlobals.vgNumDLights)
    unless (numDLights >= Constants.maxDLights) $ do
        vGlobals.vgDLights.ix numDLights .= DLightT org (V3 r g b) intensity
        vGlobals.vgNumDLights += 1

addParticle :: V3 Float -> Int -> Float -> Quake ()
addParticle org color alpha = do
    numParticles <- use (vGlobals.vgNumParticles)
    unless (numParticles >= Constants.maxParticles) $ do
        vGlobals.vgNumParticles += 1
        colorTable <- use (particleTGlobals.pColorTable)
        request $ do
            colorArray <- use pColorArray
            vertexArray <- use pVertexArray
            MSV.write colorArray numParticles ((colorTable UV.! color) .|. (truncate (alpha * 255) `shiftL` 24))
            MSV.write vertexArray (numParticles * 3 + 0) (org^._x)
            MSV.write vertexArray (numParticles * 3 + 1) (org^._y)
            MSV.write vertexArray (numParticles * 3 + 2) (org^._z)

addLightStyle :: Ref VGlobals LightStyleT -> Float -> Float -> Float -> Quake ()
addLightStyle lightStyleRef r g b =
    writeRef lightStyleRef (LightStyleT (V3 r g b) (r + g + b)) -- TODO: jake2 has boundary check here, should we do it?