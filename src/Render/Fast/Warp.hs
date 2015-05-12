{-# LANGUAGE OverloadedStrings #-}
module Render.Fast.Warp where

import Control.Lens (use, preuse, ix, (^.))
import Control.Monad (when)
import Linear (V3(..), _x, _y, _z)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Quake
import QuakeState
import qualified Constants
import qualified QCommon.Com as Com

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

subdividePolygon :: MSurfaceT -> V.Vector (V3 Float) -> Quake MSurfaceT
subdividePolygon surface verts = do
    let numVerts = (surface^.msNumEdges)

    when (numVerts > 60) $
      Com.comError Constants.errDrop ("numverts = " `B.append` BC.pack (show numVerts))

    let (mins, maxs) = boundPoly numVerts verts
    io (putStrLn "Warp.subdividePolygon") >> undefined -- TODO

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
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel
    Just model <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx

    -- convert edges back to a normal polygon
    let verts = V.generate (surface^.msNumEdges) (collectVerts model)

    subdividePolygon surface verts

  where collectVerts :: ModelT -> Int -> V3 Float
        collectVerts model idx =
          let li = (model^.mSurfEdges) V.! ((surface^.msFirstEdge) + idx)
          in if li > 0
               then ((model^.mVertexes) V.! (fromIntegral $ fst (((model^.mEdges) V.! li)^.meV)))^.mvPosition
               else ((model^.mVertexes) V.! (fromIntegral $ snd (((model^.mEdges) V.! (-li))^.meV)))^.mvPosition
