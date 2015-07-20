{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render.Fast.Warp where

import Control.Applicative (Const)
import Control.Lens (use, preuse, ix, (^.), (.=), zoom, (%=))
import Control.Monad (when, unless, liftM)
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
    ModKnownReference modelIdx <- use $ fastRenderAPIGlobals.frLoadModel
    Just model <- preuse $ fastRenderAPIGlobals.frModKnown.ix modelIdx

    -- convert edges back to a normal polygon
    let verts = V.generate (surface^.msNumEdges) (collectVerts model)

    fastRenderAPIGlobals.frWarpFace .= Just surface

    subdividePolygon (surface^.msNumEdges) verts

    use (fastRenderAPIGlobals.frWarpFace) >>= \(Just warpFace) ->
      return warpFace

  where collectVerts :: ModelT -> Int -> V3 Float
        collectVerts model idx =
          let li = (model^.mSurfEdges) V.! ((surface^.msFirstEdge) + idx)
          in if li > 0
               then ((model^.mVertexes) V.! (fromIntegral $ fst (((model^.mEdges) V.! li)^.meV)))^.mvPosition
               else ((model^.mVertexes) V.! (fromIntegral $ snd (((model^.mEdges) V.! (-li))^.meV)))^.mvPosition

rSetSky :: B.ByteString -> Float -> V3 Float -> Quake ()
rSetSky name rotate axis = do
    zoom fastRenderAPIGlobals $ do
      frSkyName .= name
      frSkyRotate .= rotate
      frSkyAxis .= axis

    glSkyMipValue <- liftM (^.cvValue) glSkyMipCVar
    glExtPalettedTextureValue <- liftM (^.cvValue) glExtPalettedTextureCVar
    colorTableEXT <- use $ fastRenderAPIGlobals.frColorTableEXT

    (updates, skyMin, skyMax) <- setSky glSkyMipValue glExtPalettedTextureValue colorTableEXT 0 6 [] 0 0
    fastRenderAPIGlobals.frSkyImages %= (V.// updates)
    fastRenderAPIGlobals.frSkyMin .= skyMin
    fastRenderAPIGlobals.frSkyMax .= skyMax

  where setSky :: Float -> Float -> Bool -> Int -> Int -> [(Int, Maybe ImageReference)] -> Float -> Float -> Quake ([(Int, Maybe ImageReference)], Float, Float)
        setSky skyMipValue extPalettedTexture colorTableEXT idx maxIdx acc skyMin skyMax
          | idx >= maxIdx = return (acc, skyMin, skyMax)
          | otherwise = do
              when (skyMipValue /= 0 || rotate /= 0) $
                glPicMipCVar >>= \picMip -> CVar.update picMip { _cvValue = (picMip^.cvValue) + 1 }

              let pathname = if colorTableEXT && extPalettedTexture /= 0
                               then "env/" `B.append` name `B.append` (suf V.! idx) `B.append` ".pcx"
                               else "env/" `B.append` name `B.append` (suf V.! idx) `B.append` ".tga"
                            
              imageRef <- Image.glFindImage pathname RenderAPIConstants.itSky
              imageRef' <- if isNothing imageRef
                                then (use $ fastRenderAPIGlobals.frNoTexture) >>= \ref -> return (Just ref)
                                else return imageRef

              if skyMipValue /= 0 || rotate /= 0
                then do
                  glPicMipCVar >>= \picMip -> CVar.update picMip { _cvValue = (picMip^.cvValue) - 1 }
                  setSky skyMipValue extPalettedTexture colorTableEXT (idx + 1) maxIdx ((idx, imageRef') : acc) (1 / 256) (255 / 256)
                else
                  setSky skyMipValue extPalettedTexture colorTableEXT (idx + 1) maxIdx ((idx, imageRef') : acc) (1 / 512) (511 / 512)

clearSkyBox :: Quake ()
clearSkyBox = do
    fastRenderAPIGlobals.frSkyMins .= (UV.replicate 6 9999, UV.replicate 6 9999)
    fastRenderAPIGlobals.frSkyMaxs .= (UV.replicate 6 (-9999), UV.replicate 6 (-9999))

drawSkyBox :: Quake ()
drawSkyBox = do
    skyRotate <- use $ fastRenderAPIGlobals.frSkyRotate

    done <- if skyRotate /= 0
              then liftM not checkIfSkyIsVisible
              else return False

    unless done $ do
      origin <- liftM (fmap realToFrac) $ use (fastRenderAPIGlobals.frOrigin)
      newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
      skyAxis <- liftM (fmap realToFrac) $ use (fastRenderAPIGlobals.frSkyAxis)

      GL.glPushMatrix
      GL.glTranslatef (origin^._x) (origin^._y) (origin^._z)
      GL.glRotatef (realToFrac $ (newRefDef^.rdTime) * skyRotate) (skyAxis^._x) (skyAxis^._y) (skyAxis^._z)

      drawSky

      GL.glPopMatrix

  where checkIfSkyIsVisible :: Quake Bool
        checkIfSkyIsVisible = do
          (skyMins0, skyMins1) <- use $ fastRenderAPIGlobals.frSkyMins
          (skyMaxs0, skyMaxs1) <- use $ fastRenderAPIGlobals.frSkyMaxs
          isSkyVisible skyMins0 skyMins1 skyMaxs0 skyMaxs1 0 6

        isSkyVisible :: UV.Vector Float -> UV.Vector Float -> UV.Vector Float -> UV.Vector Float -> Int -> Int -> Quake Bool
        isSkyVisible mins0 mins1 maxs0 maxs1 idx maxIdx
          | idx >= maxIdx = return False
          | otherwise = do
              if (mins0 UV.! idx) < (maxs0 UV.! idx) && (mins1 UV.! idx) < (maxs1 UV.! idx)
                then return True
                else isSkyVisible mins0 mins1 maxs0 maxs1 (idx + 1) maxIdx

        drawSky :: Quake ()
        drawSky = do
          skyRotate <- use $ fastRenderAPIGlobals.frSkyRotate
          (skyMins0, skyMins1) <- use $ fastRenderAPIGlobals.frSkyMins
          (skyMaxs0, skyMaxs1) <- use $ fastRenderAPIGlobals.frSkyMaxs

          if skyRotate /= 0
            then drawSkyPart (UV.replicate 6 (-1)) (UV.replicate 6 (-1)) (UV.replicate 6 1) (UV.replicate 6 1) 0 6 -- hack, forces full sky to draw when rotating
            else drawSkyPart skyMins0 skyMins1 skyMaxs0 skyMaxs1 0 6

        drawSkyPart :: UV.Vector Float -> UV.Vector Float -> UV.Vector Float -> UV.Vector Float -> Int -> Int -> Quake ()
        drawSkyPart mins0 mins1 maxs0 maxs1 idx maxIdx
          | idx >= maxIdx = return ()
          | otherwise = do
              if (mins0 UV.! idx) >= (maxs0 UV.! idx) || (mins1 UV.! idx) >= (maxs1 UV.! idx)
                then drawSkyPart mins0 mins1 maxs0 maxs1 (idx + 1) maxIdx
                else do
                  skyImages <- use $ fastRenderAPIGlobals.frSkyImages

                  let Just (ImageReference imageIdx) = skyImages V.! (skyTexOrder UV.! idx)
                  Just image <- preuse $ fastRenderAPIGlobals.frGLTextures.ix imageIdx

                  Image.glBind (fromIntegral $ image^.iTexNum)

                  GL.glBegin GL.gl_QUADS
                  makeSkyVec (mins0 UV.! idx) (mins1 UV.! idx) idx
                  makeSkyVec (mins0 UV.! idx) (maxs1 UV.! idx) idx
                  makeSkyVec (maxs0 UV.! idx) (maxs1 UV.! idx) idx
                  makeSkyVec (maxs0 UV.! idx) (mins1 UV.! idx) idx
                  GL.glEnd

                  drawSkyPart mins0 mins1 maxs0 maxs1 (idx + 1) maxIdx

stToVec :: V.Vector (V3 Int)
stToVec = V.fromList [ V3   3  (-1)   2
                     , V3 (-3)   1    2
                     , V3   1    3    2
                     , V3 (-1) (-3)   2
                     , V3 (-2) (-1)   3
                     , V3   2  (-1) (-3)
                     ]

makeSkyVec :: Float -> Float -> Int -> Quake ()
makeSkyVec s t axis = do
    skyMin <- use $ fastRenderAPIGlobals.frSkyMin
    skyMax <- use $ fastRenderAPIGlobals.frSkyMax

    let b = UV.fromList[ (s * 2300), (t * 2300), 2300 ]
        vec = stToVec V.! axis
        a' = if (vec^._x) < 0 then negate (b UV.! ((negate (vec^._x)) - 1)) else b UV.! ((vec^._x) - 1)
        b' = if (vec^._y) < 0 then negate (b UV.! ((negate (vec^._y)) - 1)) else b UV.! ((vec^._y) - 1)
        c' = if (vec^._z) < 0 then negate (b UV.! ((negate (vec^._z)) - 1)) else b UV.! ((vec^._z) - 1)
        v1 = fmap realToFrac (V3 a' b' c')
        -- avoid bilerp seam
        s' = (s + 1) * 0.5
        t' = (t + 1) * 0.5
        s'' = if | s' < skyMin -> skyMin
                 | s' > skyMax -> skyMax
                 | otherwise -> s'
        t'' = if | t' < skyMin -> skyMin
                 | t' > skyMax -> skyMax
                 | otherwise -> t'
        t''' = 1 - t''

    GL.glTexCoord2f (realToFrac s'') (realToFrac t''')
    GL.glVertex3f (v1^._x) (v1^._y) (v1^._z)
