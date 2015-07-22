{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render.Fast.Warp where

import Control.Applicative (Const)
import Control.Lens (use, preuse, ix, (^.), (.=), zoom, (%=), _1, _2)
import Control.Monad (when, unless, liftM)
import Data.IORef (readIORef)
import Data.Maybe (isNothing)
import Linear (V3(..), _x, _y, _z, V4, _xyz, dot)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Graphics.Rendering.OpenGL.Raw as GL

import Quake
import QuakeState
import CVarVariables
import qualified Constants
import qualified QCommon.Com as Com
import {-# SOURCE #-} qualified QCommon.CVar as CVar
import qualified Render.Fast.Image as Image
import qualified Render.Fast.Polygon as Polygon
import qualified Render.RenderAPIConstants as RenderAPIConstants

subdivideSize :: Float
subdivideSize = 64

skyTexOrder :: UV.Vector Int
skyTexOrder = UV.fromList [0, 2, 1, 3, 4, 5]

suf :: V.Vector B.ByteString
suf = V.fromList ["rt", "bk", "lf", "ft", "up", "dn"]

sinV :: UV.Vector Float
sinV =
    UV.fromList [           0 ,   0.19633 ,   0.392541 ,   0.588517 ,   0.784137 ,   0.979285 ,    1.17384 ,    1.3677
                ,     1.56072 ,   1.75281 ,    1.94384 ,     2.1337 ,    2.32228 ,    2.50945 ,    2.69512 ,   2.87916
                ,     3.06147 ,   3.24193 ,    3.42044 ,    3.59689 ,    3.77117 ,    3.94319 ,    4.11282 ,   4.27998
                ,     4.44456 ,   4.60647 ,    4.76559 ,    4.92185 ,    5.07515 ,    5.22538 ,    5.37247 ,   5.51632
                ,     5.65685 ,   5.79398 ,    5.92761 ,    6.05767 ,    6.18408 ,    6.30677 ,    6.42566 ,   6.54068
                ,     6.65176 ,   6.75883 ,    6.86183 ,     6.9607 ,    7.05537 ,    7.14579 ,    7.23191 ,   7.31368
                ,     7.39104 ,   7.46394 ,    7.53235 ,    7.59623 ,    7.65552 ,    7.71021 ,    7.76025 ,   7.80562
                ,     7.84628 ,   7.88222 ,    7.91341 ,    7.93984 ,    7.96148 ,    7.97832 ,    7.99036 ,   7.99759
                ,           8 ,   7.99759 ,    7.99036 ,    7.97832 ,    7.96148 ,    7.93984 ,    7.91341 ,   7.88222
                ,     7.84628 ,   7.80562 ,    7.76025 ,    7.71021 ,    7.65552 ,    7.59623 ,    7.53235 ,   7.46394
                ,     7.39104 ,   7.31368 ,    7.23191 ,    7.14579 ,    7.05537 ,     6.9607 ,    6.86183 ,   6.75883
                ,     6.65176 ,   6.54068 ,    6.42566 ,    6.30677 ,    6.18408 ,    6.05767 ,    5.92761 ,   5.79398
                ,     5.65685 ,   5.51632 ,    5.37247 ,    5.22538 ,    5.07515 ,    4.92185 ,    4.76559 ,   4.60647
                ,     4.44456 ,   4.27998 ,    4.11282 ,    3.94319 ,    3.77117 ,    3.59689 ,    3.42044 ,   3.24193
                ,     3.06147 ,   2.87916 ,    2.69512 ,    2.50945 ,    2.32228 ,     2.1337 ,    1.94384 ,   1.75281
                ,     1.56072 ,    1.3677 ,    1.17384 ,   0.979285 ,   0.784137 ,   0.588517 ,   0.392541 ,   0.19633
                , 9.79717e-16 , (-0.19633), (-0.392541), (-0.588517), (-0.784137), (-0.979285), ( -1.17384), ( -1.3677)
                ,   (-1.56072), (-1.75281), ( -1.94384), (  -2.1337), ( -2.32228), ( -2.50945), ( -2.69512), (-2.87916)
                ,   (-3.06147), (-3.24193), ( -3.42044), ( -3.59689), ( -3.77117), ( -3.94319), ( -4.11282), (-4.27998)
                ,   (-4.44456), (-4.60647), ( -4.76559), ( -4.92185), ( -5.07515), ( -5.22538), ( -5.37247), (-5.51632)
                ,   (-5.65685), (-5.79398), ( -5.92761), ( -6.05767), ( -6.18408), ( -6.30677), ( -6.42566), (-6.54068)
                ,   (-6.65176), (-6.75883), ( -6.86183), (  -6.9607), ( -7.05537), ( -7.14579), ( -7.23191), (-7.31368)
                ,   (-7.39104), (-7.46394), ( -7.53235), ( -7.59623), ( -7.65552), ( -7.71021), ( -7.76025), (-7.80562)
                ,   (-7.84628), (-7.88222), ( -7.91341), ( -7.93984), ( -7.96148), ( -7.97832), ( -7.99036), (-7.99759)
                ,   (      -8), (-7.99759), ( -7.99036), ( -7.97832), ( -7.96148), ( -7.93984), ( -7.91341), (-7.88222)
                ,   (-7.84628), (-7.80562), ( -7.76025), ( -7.71021), ( -7.65552), ( -7.59623), ( -7.53235), (-7.46394)
                ,   (-7.39104), (-7.31368), ( -7.23191), ( -7.14579), ( -7.05537), (  -6.9607), ( -6.86183), (-6.75883)
                ,   (-6.65176), (-6.54068), ( -6.42566), ( -6.30677), ( -6.18408), ( -6.05767), ( -5.92761), (-5.79398)
                ,   (-5.65685), (-5.51632), ( -5.37247), ( -5.22538), ( -5.07515), ( -4.92185), ( -4.76559), (-4.60647)
                ,   (-4.44456), (-4.27998), ( -4.11282), ( -3.94319), ( -3.77117), ( -3.59689), ( -3.42044), (-3.24193)
                ,   (-3.06147), (-2.87916), ( -2.69512), ( -2.50945), ( -2.32228), (  -2.1337), ( -1.94384), (-1.75281)
                ,   (-1.56072), ( -1.3677), ( -1.17384), (-0.979285), (-0.784137), (-0.588517), (-0.392541), (-0.19633)
                ]

subdividePolygon :: Int -> V.Vector (V3 Float) -> Quake ()
subdividePolygon numVerts verts = do
    when (numVerts > 60) $
      Com.comError Constants.errDrop ("numverts = " `B.append` BC.pack (show numVerts))

    let (mins, maxs) = boundPoly numVerts verts
        verts' = verts `V.snoc` (verts V.! 0)

    done <- subdivide verts' mins maxs 0

    unless done $ do
      polyRef@(GLPolyReference polyIdx) <- Polygon.create (numVerts + 2)
      Just surface <- use $ fastRenderAPIGlobals.frWarpFace

      use (fastRenderAPIGlobals.frPolygonCache) >>= \polygonCache -> do
        io $ do
          poly <- MV.read polygonCache polyIdx
          MV.write polygonCache polyIdx poly { _glpNext = surface^.msPolys }

      fastRenderAPIGlobals.frWarpFace .= Just (surface { _msPolys = Just polyRef })

      (total, totalS, totalT) <- countTotals polyRef (surface^.msTexInfo.mtiVecs) (V3 0 0 0) 0 0 0

      let scale = 1 / (fromIntegral numVerts)

      Polygon.setPolyX polyRef 0 ((total^._x) * scale)
      Polygon.setPolyY polyRef 0 ((total^._y) * scale)
      Polygon.setPolyZ polyRef 0 ((total^._z) * scale)
      Polygon.setPolyS1 polyRef 0 (totalS * scale)
      Polygon.setPolyT1 polyRef 0 (totalT * scale)

      Polygon.getPolyX polyRef 1 >>= Polygon.setPolyX polyRef (numVerts + 1)
      Polygon.getPolyY polyRef 1 >>= Polygon.setPolyY polyRef (numVerts + 1)
      Polygon.getPolyZ polyRef 1 >>= Polygon.setPolyZ polyRef (numVerts + 1)
      Polygon.getPolyS1 polyRef 1 >>= Polygon.setPolyS1 polyRef (numVerts + 1)
      Polygon.getPolyT1 polyRef 1 >>= Polygon.setPolyT1 polyRef (numVerts + 1)
      Polygon.getPolyS2 polyRef 1 >>= Polygon.setPolyS2 polyRef (numVerts + 1)
      Polygon.getPolyT2 polyRef 1 >>= Polygon.setPolyT2 polyRef (numVerts + 1)

  where subdivide :: V.Vector (V3 Float) -> V3 Float -> V3 Float -> Int -> Quake Bool
        subdivide verts' mins maxs idx
          | idx >= 3 = return False
          | otherwise = do
              let access = if | idx == 0 -> _x
                              | idx == 1 -> _y
                              | idx == 2 -> _z
                              | otherwise -> undefined -- shouldn't happen
                  m = ((mins^.access) + (maxs^.access)) * 0.5
                  tmp :: Int = floor (m / subdivideSize + 0.5)
                  m' = subdivideSize * fromIntegral tmp

              if (maxs^.access) - m' < 8 || m' - (mins^.access) < 8
                then subdivide verts' mins maxs (idx + 1)
                else do
                  let dist = V.generate (numVerts + 1) (buildDist verts' access m')
                      (front, back) = buildFrontAndBack verts' dist V.empty V.empty 0 numVerts

                  subdividePolygon (V.length front) front
                  subdividePolygon (V.length back) back

                  return True

        buildDist :: V.Vector (V3 Float) -> ((Float -> Const Float Float) -> V3 Float -> Const Float (V3 Float)) -> Float -> Int -> Float
        buildDist verts' access m idx =
          if idx == numVerts
            then ((verts' V.! 0)^.access) - m
            else ((verts' V.! idx)^.access) - m

        buildFrontAndBack :: V.Vector (V3 Float) -> V.Vector Float -> V.Vector (V3 Float) -> V.Vector (V3 Float) -> Int -> Int -> (V.Vector (V3 Float), V.Vector (V3 Float))
        buildFrontAndBack verts' dist front back idx maxIdx
          | idx >= maxIdx = (front, back)
          | otherwise = 
              let v = verts' V.! idx
                  front' = if dist V.! idx >= 0
                             then front `V.snoc` v
                             else front
                  back' = if dist V.! idx <= 0
                            then back `V.snoc` v
                            else back
              in if dist V.! idx == 0 || dist V.! (idx + 1) == 0
                   then buildFrontAndBack verts' dist front' back' (idx + 1) maxIdx
                   else let a = dist V.! idx > 0
                            b = dist V.! (idx + 1) > 0
                        in if a /= b -- clip point
                             then let frac = (dist V.! idx) / ((dist V.! idx) - (dist V.! (idx + 1)))
                                      v1 = verts' V.! (idx + 1)
                                      fb = v + (fmap (* frac) (v1 - v))
                                      front'' = front' `V.snoc` fb
                                      back'' = back' `V.snoc` fb
                                  in buildFrontAndBack verts' dist front'' back'' (idx + 1) maxIdx
                             else buildFrontAndBack verts' dist front' back' (idx + 1) maxIdx

        countTotals :: GLPolyReference -> (V4 Float, V4 Float) -> V3 Float -> Float -> Float -> Int -> Quake (V3 Float, Float, Float)
        countTotals polyRef vecs total totalS totalT idx
          | idx >= numVerts = return (total, totalS, totalT)
          | otherwise = do
              let v = verts V.! idx

              Polygon.setPolyX polyRef (idx + 1) (v^._x)
              Polygon.setPolyY polyRef (idx + 1) (v^._y)
              Polygon.setPolyZ polyRef (idx + 1) (v^._z)

              let s = v `dot` ((fst vecs)^._xyz)
                  t = v `dot` ((snd vecs)^._xyz)

              Polygon.setPolyS1 polyRef (idx + 1) s
              Polygon.setPolyT1 polyRef (idx + 1) t

              countTotals polyRef vecs (total + v) (totalS + s) (totalT + t) (idx + 1)

boundPoly :: Int -> V.Vector (V3 Float) -> (V3 Float, V3 Float)
boundPoly numVerts verts = findMinMax 0 (V3 9999 9999 9999) (V3 (-9999) (-9999) (-9999))
  where findMinMax :: Int -> V3 Float -> V3 Float -> (V3 Float, V3 Float)
        findMinMax idx mins maxs
          | idx >= numVerts = (mins, maxs)
          | otherwise =
              let v = verts V.! idx
                  mina = if (v^._x) < (mins^._x) then v^._x else mins^._x
                  minb = if (v^._y) < (mins^._y) then v^._y else mins^._y
                  minc = if (v^._z) < (mins^._z) then v^._z else mins^._z
                  maxa = if (v^._x) > (maxs^._x) then v^._x else maxs^._x
                  maxb = if (v^._y) > (maxs^._y) then v^._y else maxs^._y
                  maxc = if (v^._z) > (maxs^._z) then v^._z else maxs^._z
              in findMinMax (idx + 1) (V3 mina minb minc) (V3 maxa maxb maxc)

{-
- GL_SubdivideSurface
- Breaks a polygon up along axial 64 unit
- boundaries so that turbulent and sky warps
- can be done reasonably.
-}
glSubdivideSurface :: MSurfaceT -> Quake MSurfaceT
glSubdivideSurface surface = do
    loadModelRef <- use $ fastRenderAPIGlobals.frLoadModel
    model <- io $ readIORef loadModelRef

    -- convert edges back to a normal polygon
    -- let verts = V.generate (surface^.msNumEdges) (collectVerts model)
    verts <- V.generateM (surface^.msNumEdges) (collectVerts model)

    fastRenderAPIGlobals.frWarpFace .= Just surface

    subdividePolygon (surface^.msNumEdges) verts

    use (fastRenderAPIGlobals.frWarpFace) >>= \(Just warpFace) ->
      return warpFace

  where collectVerts :: ModelT -> Int -> Quake (V3 Float)
        collectVerts loadModel idx = do
          let lindex = (loadModel^.mSurfEdges) V.! ((surface^.msFirstEdge) + idx)

          if lindex > 0
            then do
              let edgeRef = (loadModel^.mEdges) V.! lindex
              edge <- io $ readIORef edgeRef
              let vecRef = (loadModel^.mVertexes) V.! (fromIntegral $ edge^.meV._1)
              vec <- io $ readIORef vecRef
              return (vec^.mvPosition)
            else do
              let edgeRef = (loadModel^.mEdges) V.! (negate lindex)
              edge <- io $ readIORef edgeRef
              let vecRef = (loadModel^.mVertexes) V.! (fromIntegral $ edge^.meV._2)
              vec <- io $ readIORef vecRef
              return (vec^.mvPosition)
