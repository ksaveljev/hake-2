{-# LANGUAGE MultiWayIf #-}
module Render.Fast.Light where

import Control.Lens ((^.), preuse, use, (.=), ix, _1, _2)
import Control.Monad (when, liftM)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.IORef (IORef, readIORef, modifyIORef')
import Data.Maybe (fromJust)
import Linear (V3, dot)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import CVarVariables
import qualified Constants

dLightCutoff :: Float
dLightCutoff = 64

rPushDLights :: Quake ()
rPushDLights = do
    flashBlendValue <- liftM (^.cvValue) glFlashBlendCVar

    when (flashBlendValue == 0) $ do
      frameCount <- use $ fastRenderAPIGlobals.frFrameCount
      newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
      Just worldModelRef <- use $ fastRenderAPIGlobals.frWorldModel
      worldModel <- io $ readIORef worldModelRef

      fastRenderAPIGlobals.frDLightFrameCount .= frameCount + 1 -- because the count hasn't advanced yet for this frame

      markLights newRefDef worldModel 0 (newRefDef^.rdNumDLights)

  where markLights :: RefDefT -> ModelT -> Int -> Int -> Quake ()
        markLights newRefDef worldModel idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let light = (newRefDef^.rdDLights) V.! idx
              rMarkLights light (1 `shiftL` idx) (MNodeChildReference $ (worldModel^.mNodes) V.! 0)
              markLights newRefDef worldModel (idx + 1) maxIdx

rMarkLights :: DLightT -> Int -> MNodeChild -> Quake ()
rMarkLights _ _ (MLeafChildReference _) = return ()
rMarkLights light bit (MNodeChildReference nodeRef) = do
    node <- io $ readIORef nodeRef
    splitPlane <- io $ readIORef (node^.mnPlane)

    let dist = (light^.dlOrigin) `dot` (splitPlane^.cpNormal) - (splitPlane^.cpDist)

    if | dist > (light^.dlIntensity) - dLightCutoff -> rMarkLights light bit (node^.mnChildren._1)
       | dist < dLightCutoff - (light^.dlIntensity) -> rMarkLights light bit (node^.mnChildren._2)
       | otherwise -> do
           Just worldModelRef <- use $ fastRenderAPIGlobals.frWorldModel
           worldModel <- io $ readIORef worldModelRef

           markPolygons worldModel node 0 (node^.mnNumSurfaces)

           rMarkLights light bit (node^.mnChildren._1)
           rMarkLights light bit (node^.mnChildren._2)

  where markPolygons :: ModelT -> MNodeT -> Int -> Int -> Quake ()
        markPolygons worldModel node idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              let surfRef = (worldModel^.mSurfaces) V.! ((node^.mnFirstSurface) + idx)
              surf <- io $ readIORef surfRef
              plane <- io $ readIORef (fromJust $ surf^.msPlane)

              let dist = (light^.dlOrigin) `dot` (plane^.cpNormal) - (plane^.cpDist)
                  sidebit = if dist >= 0 then 0 else Constants.surfPlaneBack

              if (surf^.msFlags) .&. Constants.surfPlaneBack /= sidebit
                then markPolygons worldModel node (idx + 1) maxIdx
                else do
                  dLightFrameCount <- use $ fastRenderAPIGlobals.frDLightFrameCount

                  when ((surf^.msDLightFrame) /= dLightFrameCount) $
                    io $ modifyIORef' surfRef (\v -> v { _msDLightBits = 0
                                                       , _msDLightFrame = dLightFrameCount
                                                       })

                  io $ modifyIORef' surfRef (\v -> v { _msDLightBits = (v^.msDLightBits) .|. bit })

                  markPolygons worldModel node (idx + 1) maxIdx

rRenderDLights :: Quake ()
rRenderDLights = do
    io (putStrLn "Light.rRenderDLights") >> undefined -- TODO

rLightPoint :: V3 Float -> Quake (V3 Float)
rLightPoint _ = do
    io (putStrLn "Light.rLightPoint") >> undefined -- TODO

rSetCacheState :: IORef MSurfaceT -> Quake ()
rSetCacheState surfRef = do
    surf <- io $ readIORef surfRef
    newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
    let updates = updateCachedLight surf newRefDef 0 Constants.maxLightMaps []
    io $ modifyIORef' surfRef (\v -> v { _msCachedLight = (surf^.msCachedLight) UV.// updates })

  where updateCachedLight :: MSurfaceT -> RefDefT -> Int -> Int -> [(Int, Float)] -> [(Int, Float)]
        updateCachedLight surf newRefDef idx maxIdx acc
          | idx >= maxIdx = acc
          | otherwise =
              let f = (surf^.msStyles) `B.index` idx
              in if f == 255
                   then acc
                   else let w = ((newRefDef^.rdLightStyles) V.! (fromIntegral f))^.lsWhite
                        in updateCachedLight surf newRefDef (idx + 1) maxIdx ((idx, w) : acc)
