module Render.Fast.Light
    ( rBuildLightMap
    , rLightPoint
    , rPushDLights
    , rRenderDLights
    , rSetCacheState
    ) where

import           Control.Lens                (use, (^.))
import           Control.Monad               (void)
import           Control.Monad.ST            (ST, runST)
import qualified Data.ByteString             as B
import qualified Data.Vector                 as V
import qualified Data.Vector.Storable        as SV
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import           Data.Word                   (Word8)
import           Linear                      (V3)

import           Client.LightStyleT
import           Client.RefDefT
import qualified Constants
import           QuakeRef
import           QuakeState
import           Render.MSurfaceT
import           Types

rPushDLights :: Quake ()
rPushDLights = error "Light.rPushDLights" -- TODO

rRenderDLights :: Quake ()
rRenderDLights = error "Light.rRenderDLights" -- TODO

rLightPoint :: V3 Float -> Quake (V3 Float)
rLightPoint = error "Light.rLightPoint" -- TODO

rSetCacheState :: Ref MSurfaceT -> Quake ()
rSetCacheState surfRef = do
    surf <- readRef surfRef
    newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
    return $ runST $ do
        lights <- UV.unsafeThaw (surf^.msCachedLight)
        cacheLights newRefDef surf lights 0 Constants.maxLightMaps
        void (UV.unsafeFreeze lights)

cacheLights :: RefDefT -> MSurfaceT -> UV.MVector s Float -> Int -> Int -> ST s ()
cacheLights newRefDef surf lights idx maxIdx
    | idx >= maxIdx = return ()
    | style == 255 = return ()
    | otherwise = do
        MUV.write lights idx (((newRefDef^.rdLightStyles) V.! (fromIntegral style))^.lsWhite)
        cacheLights newRefDef surf lights (idx + 1) maxIdx
  where
    style = (surf^.msStyles) `B.index` idx

rBuildLightMap :: Ref MSurfaceT -> SV.Vector Word8 -> Int -> Int -> Quake ()
rBuildLightMap = error "Light.rBuildLightMap" -- TODO