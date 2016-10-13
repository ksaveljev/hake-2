module Render.Fast.Warp
    ( glSubdivideSurface
    , rSetSky
    ) where

import           Control.Applicative (Const)
import           Control.Lens        (use, (^.), (.=), (&), (.~), _1, _2)
import           Control.Monad       (when, unless)
import qualified Data.ByteString     as B
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV
import           Linear              (V3(..), V4, dot, _x, _y, _z, _xyz)

import qualified Constants
import qualified QCommon.Com         as Com
import           QuakeRef
import           QuakeState
import qualified Render.Fast.Polygon as Polygon
import           Render.GLPolyT
import           Render.MEdgeT
import           Render.ModelT
import           Render.MSurfaceT
import           Render.MTexInfoT
import           Render.MVertexT
import           Types
import           Util.Binary        (encode)

subdivideSize :: Float
subdivideSize = 64

rSetSky :: B.ByteString -> Float -> V3 Float -> Quake ()
rSetSky = error "Warp.rSetSky" -- TODO

glSubdivideSurface :: Ref MSurfaceT -> Quake ()
glSubdivideSurface surfRef = do
    loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
    surf <- readRef surfRef
    model <- readRef loadModelRef
    let verts = V.generate (surf^.msNumEdges) (collectVerts surf model)
    fastRenderAPIGlobals.frWarpFace .= Just surfRef
    subdividePolygon (surf^.msNumEdges) verts

collectVerts :: MSurfaceT -> ModelT -> Int -> V3 Float
collectVerts surf loadModel idx
    | lindex > 0 =
        let edge = (loadModel^.mEdges) V.! lindex
            vec = (loadModel^.mVertexes) V.! (fromIntegral (edge^.meV._1))
        in vec^.mvPosition
    | otherwise =
        let edge = (loadModel^.mEdges) V.! (negate lindex)
            vec = (loadModel^.mVertexes) V.! (fromIntegral (edge^.meV._2))
        in vec^.mvPosition
  where
    lindex = (loadModel^.mSurfEdges) UV.! ((surf^.msFirstEdge) + idx)

subdividePolygon :: Int -> V.Vector (V3 Float) -> Quake ()
subdividePolygon numVerts verts = do
    when (numVerts > 60) $
        Com.comError Constants.errDrop ("numverts = " `B.append` encode numVerts)
    done <- subdivide verts' numVerts mins maxs 0
    unless done $ do
        polyRef <- Polygon.create (numVerts + 2)
        surfRef <- use (fastRenderAPIGlobals.frWarpFace)
        proceedWarp polyRef surfRef verts numVerts
  where
    (mins, maxs) = boundPoly numVerts verts
    verts' = verts `V.snoc` (verts V.! 0)

proceedWarp :: Ref GLPolyT -> Maybe (Ref MSurfaceT) -> V.Vector (V3 Float) -> Int -> Quake ()
proceedWarp _ Nothing _ _ = Com.fatalError "Warp.proceedWarp surface is Nothing"
proceedWarp polyRef (Just surfRef) verts numVerts = do
    surf <- readRef surfRef
    modifyRef polyRef (\v -> v & glpNext .~ (surf^.msPolys))
    modifyRef surfRef (\v -> v & msPolys .~ Just polyRef)
    texInfo <- readRef (surf^.msTexInfo)
    (total, totalS, totalT) <- countTotals polyRef (texInfo^.mtiVecs) verts numVerts (V3 0 0 0) 0 0 0
    Polygon.setPolyX  polyRef 0 ((total^._x) * scale)
    Polygon.setPolyY  polyRef 0 ((total^._y) * scale)
    Polygon.setPolyZ  polyRef 0 ((total^._z) * scale)
    Polygon.setPolyS1 polyRef 0 (totalS * scale)
    Polygon.setPolyT1 polyRef 0 (totalT * scale)
    Polygon.getPolyX  polyRef 1 >>= Polygon.setPolyX  polyRef (numVerts + 1)
    Polygon.getPolyY  polyRef 1 >>= Polygon.setPolyY  polyRef (numVerts + 1)
    Polygon.getPolyZ  polyRef 1 >>= Polygon.setPolyZ  polyRef (numVerts + 1)
    Polygon.getPolyS1 polyRef 1 >>= Polygon.setPolyS1 polyRef (numVerts + 1)
    Polygon.getPolyT1 polyRef 1 >>= Polygon.setPolyT1 polyRef (numVerts + 1)
    Polygon.getPolyS2 polyRef 1 >>= Polygon.setPolyS2 polyRef (numVerts + 1)
    Polygon.getPolyT2 polyRef 1 >>= Polygon.setPolyT2 polyRef (numVerts + 1)
  where
    scale = 1 / (fromIntegral numVerts)

subdivide :: V.Vector (V3 Float) -> Int -> V3 Float -> V3 Float -> Int -> Quake Bool
subdivide verts numVerts mins maxs idx
    | idx >= 3 = return False
    | otherwise = doSubdivide verts numVerts mins maxs idx

doSubdivide :: V.Vector (V3 Float) -> Int -> V3 Float -> V3 Float -> Int -> Quake Bool
doSubdivide verts numVerts mins maxs idx
    | (maxs^.access) - m' < 8 || m' - (mins^.access) < 8 =
        subdivide verts numVerts mins maxs (idx + 1)
    | otherwise = do
        subdividePolygon (V.length front) front
        subdividePolygon (V.length back) back
        return True
  where
    access | idx == 0 = _x
           | idx == 1 = _y
           | idx == 2 = _z
           | otherwise = undefined -- shouldn't happen
    m = ((mins^.access) + (maxs^.access)) * 0.5
    tmp = floor (m / subdivideSize + 0.5) :: Int
    m' = subdivideSize * fromIntegral tmp
    dist = V.generate (numVerts + 1) (buildDist verts numVerts access m')
    (front, back) = buildFrontAndBack verts dist V.empty V.empty 0 numVerts

boundPoly :: Int -> V.Vector (V3 Float) -> (V3 Float, V3 Float)
boundPoly numVerts verts =
    findMinMax 0 (V3 9999 9999 9999) (V3 (-9999) (-9999) (-9999))
  where
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

buildDist :: V.Vector (V3 Float) -> Int -> ((Float -> Const Float Float) -> V3 Float -> Const Float (V3 Float)) -> Float -> Int -> Float
buildDist verts numVerts access m idx
    | idx == numVerts = ((verts V.! 0)^.access) - m
    | otherwise = ((verts V.! idx)^.access) - m

-- TODO: this is an old implementation, could use some refactoring
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

-- TODO: old implementation, could use some refactoring
countTotals :: Ref GLPolyT -> (V4 Float, V4 Float) -> V.Vector (V3 Float) -> Int -> V3 Float -> Float -> Float -> Int -> Quake (V3 Float, Float, Float)
countTotals polyRef vecs verts numVerts total totalS totalT idx
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
        countTotals polyRef vecs verts numVerts (total + v) (totalS + s) (totalT + t) (idx + 1)