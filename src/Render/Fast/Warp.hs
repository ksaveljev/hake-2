module Render.Fast.Warp
    ( clearSkyBox
    , drawSkyBox
    , glSubdivideSurface
    , rAddSkySurface
    , rSetSky
    ) where

import           Control.Applicative   (Const)
import           Control.Lens          (use, ix, (^.), (.=), (%=), (&), (.~), (+~), (-~), _1, _2)
import           Control.Monad         (when, unless)
import qualified Data.ByteString       as B
import           Data.IORef            (IORef, readIORef, modifyIORef')
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV
import qualified Data.Vector.Unboxed   as UV
import qualified Graphics.GL           as GL
import           Linear                (V3(..), V4, dot, _x, _y, _z, _xyz)
import           System.IO.Unsafe      (unsafePerformIO)

import           Client.RefDefT
import qualified Constants
import           Game.CVarT
import qualified QCommon.Com           as Com
import qualified QCommon.CVar          as CVar
import           QCommon.CVarVariables
import           QuakeRef
import           QuakeState
import qualified Render.Fast.Image     as Image
import qualified Render.Fast.Polygon   as Polygon
import           Render.GLPolyT
import           Render.ImageT
import           Render.MEdgeT
import           Render.ModelT
import           Render.MSurfaceT
import           Render.MTexInfoT
import           Render.MVertexT
import           Types
import           Util.Binary           (encode)

sideFront :: Int
sideFront = 0

sideBack :: Int
sideBack = 1

sideOn :: Int
sideOn = 2

onEpsilon :: Float
onEpsilon = 0.1 -- point on plane side epsilon
	
subdivideSize :: Float
subdivideSize = 64

maxClipVerts :: Int
maxClipVerts = 64

skyTexOrder :: UV.Vector Int
skyTexOrder = UV.fromList [0, 2, 1, 3, 4, 5]

suf :: V.Vector B.ByteString
suf = V.fromList ["rt", "bk", "lf", "ft", "up", "dn"]

vecToSt :: V.Vector (V3 Int)
vecToSt = V.fromList
    [ V3 (-2)   3    1
    , V3   2    3  (-1)
    , V3   1    3    2
    , V3 (-1)   3  (-2)
    , V3 (-2) (-1)   3
    , V3 (-2)   1  (-3)
    ]

skyClip :: V.Vector (V3 Float)
skyClip = V.fromList
    [ V3   1    1    0
    , V3   1  (-1)   0
    , V3   0  (-1)   1
    , V3   0    1    1
    , V3   1    0    1
    , V3 (-1)   0    1
    ]

warpVerts :: MV.IOVector (V3 Float)
warpVerts = unsafePerformIO (MV.new maxClipVerts)

sides :: MV.IOVector Int
sides = unsafePerformIO (MV.new maxClipVerts)

dists :: MV.IOVector Float
dists = unsafePerformIO (MV.new maxClipVerts)

newv0 :: MV.IOVector (V3 Float)
newv0 = unsafePerformIO (MV.new maxClipVerts)

newv1 :: MV.IOVector (V3 Float)
newv1 = unsafePerformIO (MV.new maxClipVerts)

rSetSky :: B.ByteString -> Float -> V3 Float -> Quake ()
rSetSky name rotate axis = do
    fastRenderAPIGlobals %= (\v -> v & frSkyName .~ name
                                     & frSkyRotate .~ rotate
                                     & frSkyAxis .~ axis)
    glSkyMip <- fmap (^.cvValue) glSkyMipCVar
    glExtPalettedTexture <- fmap (^.cvValue) glExtPalettedTextureCVar
    colorTableEXT <- use (fastRenderAPIGlobals.frColorTableEXT)
    (skyMin, skyMax) <- setSky name rotate glSkyMip glExtPalettedTexture colorTableEXT 0 0 0 6
    fastRenderAPIGlobals %= (\v -> v & frSkyMin .~ skyMin
                                     & frSkyMax .~ skyMax)

setSky :: B.ByteString -> Float -> Float -> Float -> Bool -> Float -> Float -> Int -> Int -> Quake (Float, Float)
setSky name rotate glSkyMip glExtPalettedTexture colorTableEXT skyMin skyMax idx maxIdx
    | idx >= maxIdx = return (skyMin, skyMax)
    | otherwise = do
        when (glSkyMip /= 0 || rotate /= 0) $ do
            glPicMip <- glPicMipCVar
            CVar.update (glPicMip & cvValue +~ 1)
        imageRef <- Image.glFindImage pathname Constants.itSky
        imageRef' <- maybe (use (fastRenderAPIGlobals.frNoTexture)) return imageRef
        fastRenderAPIGlobals.frSkyImages.ix idx .= Just imageRef'
        proceedSetSky
  where
    pathname
        | colorTableEXT && glExtPalettedTexture /= 0 = B.concat ["env/", name, suf V.! idx, ".pcx"]
        | otherwise = B.concat ["env/", name, suf V.! idx, ".tga"]
    proceedSetSky
        | glSkyMip /= 0 || rotate /= 0 = do
            glPicMip <- glPicMipCVar
            CVar.update (glPicMip & cvValue -~ 1)
            setSky name rotate glSkyMip glExtPalettedTexture colorTableEXT (1.0 / 256.0) (255.0 / 256.0) (idx + 1) maxIdx
        | otherwise =
            setSky name rotate glSkyMip glExtPalettedTexture colorTableEXT (1.0 / 512.0) (511.0 / 512.0) (idx + 1) maxIdx

glSubdivideSurface :: IORef MSurfaceT -> Quake ()
glSubdivideSurface surfRef = do
    loadModelRef <- use (fastRenderAPIGlobals.frLoadModel)
    surf <- io (readIORef surfRef)
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

proceedWarp :: Ref GLPolyT -> Maybe (IORef MSurfaceT) -> V.Vector (V3 Float) -> Int -> Quake ()
proceedWarp _ Nothing _ _ = Com.fatalError "Warp.proceedWarp surface is Nothing"
proceedWarp polyRef (Just surfRef) verts numVerts = do
    surf <- io (readIORef surfRef)
    modifyRef polyRef (\v -> v & glpNext .~ (surf^.msPolys))
    io $ modifyIORef' surfRef (\v -> v & msPolys .~ Just polyRef)
    texInfo <- io (readIORef (surf^.msTexInfo))
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

clearSkyBox :: Quake ()
clearSkyBox = do
    fastRenderAPIGlobals.frSkyMins .= (UV.replicate 6 9999, UV.replicate 6 9999)
    fastRenderAPIGlobals.frSkyMaxs .= (UV.replicate 6 (-9999), UV.replicate 6 (-9999))

drawSkyBox :: Quake ()
drawSkyBox = do
    skyRotate <- use (fastRenderAPIGlobals.frSkyRotate)
    nothingToDo <- checkIfNothingToDo skyRotate
    unless nothingToDo $ do
        origin <- fmap (fmap realToFrac) (use (fastRenderAPIGlobals.frOrigin))
        skyAxis <- fmap (fmap realToFrac) (use (fastRenderAPIGlobals.frSkyAxis))
        newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
        io $ do
            GL.glPushMatrix
            GL.glTranslatef (origin^._x) (origin^._y) (origin^._z)
            GL.glRotatef (realToFrac ((newRefDef^.rdTime) * skyRotate)) (skyAxis^._x) (skyAxis^._y) (skyAxis^._z)
        drawSky
        io (GL.glPopMatrix)
  where
    checkIfNothingToDo skyRotate
        | skyRotate /= 0 = fmap not checkIfSkyIsVisible
        | otherwise = return False
    checkIfSkyIsVisible = do
        (skyMins0, skyMins1) <- use (fastRenderAPIGlobals.frSkyMins)
        (skyMaxs0, skyMaxs1) <- use (fastRenderAPIGlobals.frSkyMaxs)
        isSkyVisible skyMins0 skyMins1 skyMaxs0 skyMaxs1 0 6
    isSkyVisible mins0 mins1 maxs0 maxs1 idx maxIdx
        | idx >= maxIdx = return False
        | (mins0 UV.! idx) < (maxs0 UV.! idx) && (mins1 UV.! idx) < (maxs1 UV.! idx) = return True
        | otherwise = isSkyVisible mins0 mins1 maxs0 maxs1 (idx + 1) maxIdx
    drawSky = do
        skyRotate <- use (fastRenderAPIGlobals.frSkyRotate)
        (skyMins0, skyMins1) <- use (fastRenderAPIGlobals.frSkyMins)
        (skyMaxs0, skyMaxs1) <- use (fastRenderAPIGlobals.frSkyMaxs)
        doDrawSky skyRotate skyMins0 skyMins1 skyMaxs0 skyMaxs1
    doDrawSky skyRotate mins0 mins1 maxs0 maxs1
        | skyRotate /= 0 = drawSkyPart (UV.replicate 6 (-1)) (UV.replicate 6 (-1)) (UV.replicate 6 1) (UV.replicate 6 1) 0 6 -- hack, forces full sky to draw when rotating
        | otherwise = drawSkyPart mins0 mins1 maxs0 maxs1 0 6
    drawSkyPart mins0 mins1 maxs0 maxs1 idx maxIdx
        | idx >= maxIdx = return ()
        | (mins0 UV.! idx) >= (maxs0 UV.! idx) || (mins1 UV.! idx) >= (maxs1 UV.! idx) =
            drawSkyPart mins0 mins1 maxs0 maxs1 (idx + 1) maxIdx
        | otherwise = do
            skyImages <- use (fastRenderAPIGlobals.frSkyImages)
            doDrawSkyPart mins0 mins1 maxs0 maxs1 idx (skyImages V.! (skyTexOrder UV.! idx))
            drawSkyPart mins0 mins1 maxs0 maxs1 (idx + 1) maxIdx
    doDrawSkyPart _ _ _ _ _ Nothing =
        Com.fatalError "Warp.drawSkyBox one of the images in skyImages is Nothing"
    doDrawSkyPart mins0 mins1 maxs0 maxs1 idx (Just imageRef) = do
        image <- readRef imageRef
        Image.glBind (fromIntegral (image^.iTexNum))
        skyMin <- use (fastRenderAPIGlobals.frSkyMin)
        skyMax <- use (fastRenderAPIGlobals.frSkyMax)
        io $ do
            GL.glBegin GL.GL_QUADS
            makeSkyVec skyMin skyMax (mins0 UV.! idx) (mins1 UV.! idx) idx
            makeSkyVec skyMin skyMax (mins0 UV.! idx) (maxs1 UV.! idx) idx
            makeSkyVec skyMin skyMax (maxs0 UV.! idx) (maxs1 UV.! idx) idx
            makeSkyVec skyMin skyMax (maxs0 UV.! idx) (mins1 UV.! idx) idx
            GL.glEnd

makeSkyVec :: Float -> Float -> Float -> Float -> Int -> IO ()
makeSkyVec skyMin skyMax s t axis = do
    GL.glTexCoord2f (realToFrac s'') (realToFrac t''')
    GL.glVertex3f (v1^._x) (v1^._y) (v1^._z)
  where
    b = UV.fromList[ (s * 2300), (t * 2300), 2300 ]
    vec = stToVec V.! axis
    a' = if (vec^._x) < 0 then negate (b UV.! ((negate (vec^._x)) - 1)) else b UV.! ((vec^._x) - 1)
    b' = if (vec^._y) < 0 then negate (b UV.! ((negate (vec^._y)) - 1)) else b UV.! ((vec^._y) - 1)
    c' = if (vec^._z) < 0 then negate (b UV.! ((negate (vec^._z)) - 1)) else b UV.! ((vec^._z) - 1)
    v1 = fmap realToFrac (V3 a' b' c')
    -- avoid bilerp seam
    s' = (s + 1) * 0.5
    t' = (t + 1) * 0.5
    s'' | s' < skyMin = skyMin
        | s' > skyMax = skyMax
        | otherwise = s'
    t'' | t' < skyMin = skyMin
        | t' > skyMax = skyMax
        | otherwise = t'
    t''' = 1 - t''

stToVec :: V.Vector (V3 Int)
stToVec = V.fromList [ V3   3  (-1)   2
                     , V3 (-3)   1    2
                     , V3   1    3    2
                     , V3 (-1) (-3)   2
                     , V3 (-2) (-1)   3
                     , V3   2  (-1) (-3)
                     ]

rAddSkySurface :: IORef MSurfaceT -> Quake ()
rAddSkySurface surfRef = do
    surf <- io (readIORef surfRef)
    origin <- use (fastRenderAPIGlobals.frOrigin)
    -- calculate vertex values for sky box
    calculate origin (surf^.msPolys)
  where
    calculate _ Nothing = return ()
    calculate origin (Just polyRef) = do
        poly <- readRef polyRef
        mapM_ (constructVerts polyRef origin) [0..(poly^.glpNumVerts)-1]
        clipSkyPolygon (poly^.glpNumVerts) warpVerts 0
        calculate origin (poly^.glpNext)
    constructVerts polyRef origin idx = do
        x <- Polygon.getPolyX polyRef idx
        y <- Polygon.getPolyY polyRef idx
        z <- Polygon.getPolyZ polyRef idx
        io (MV.write warpVerts idx (V3 (x - (origin^._x)) (y - (origin^._y)) (z - (origin^._z))))

clipSkyPolygon :: Int -> MV.IOVector (V3 Float) -> Int -> Quake ()
clipSkyPolygon nump vecs stage
    | stage == 6 = do
        checkMaxClipVertsError
        drawSkyPolygon nump vecs
    | otherwise = do
        checkMaxClipVertsError
        (front, back) <- io (checkFrontAndBack vecs (skyClip V.! stage) False False 0 nump)
        proceedClipSkyPolygon front back
        error "Warp.clipSkyPolygon" -- TODO
  where
    checkMaxClipVertsError =
        when (nump > maxClipVerts - 2) $
            Com.comError Constants.errDrop "ClipSkyPolygon: MAX_CLIP_VERTS"
    proceedClipSkyPolygon front back
        | not front || not back = clipSkyPolygon nump vecs (stage + 1) -- not clipped
        | otherwise = doClipSkyPolygon nump vecs stage

checkFrontAndBack :: MV.IOVector (V3 Float) -> V3 Float -> Bool -> Bool -> Int -> Int -> IO (Bool, Bool)
checkFrontAndBack vecs norm front back idx maxIdx
    | idx >= maxIdx = return (front, back)
    | otherwise = do
        v <- MV.read vecs idx
        let d = v `dot` norm
        MV.write dists idx d
        doCheckFrontAndBack d
  where
    doCheckFrontAndBack d
        | d > onEpsilon = do
            MV.write sides idx sideFront
            checkFrontAndBack vecs norm True back (idx + 1) maxIdx
        | d < negate onEpsilon = do
            MV.write sides idx sideBack
            checkFrontAndBack vecs norm front True (idx + 1) maxIdx
        | otherwise = do
            MV.write sides idx sideOn
            checkFrontAndBack vecs norm front back (idx + 1) maxIdx

doClipSkyPolygon :: Int -> MV.IOVector (V3 Float) -> Int -> Quake ()
doClipSkyPolygon nump vecs stage = do
    io $ do
        s <- MV.read sides 0
        MV.write sides nump s
        d <- MV.read dists 0
        MV.write dists nump d
        v <- MV.read vecs 0
        MV.write vecs nump v
    (newc0, newc1) <- io (clipStuff vecs 0 0 0 nump)
    -- continue
    clipSkyPolygon newc0 newv0 (stage + 1)
    clipSkyPolygon newc1 newv1 (stage + 1)

clipStuff :: MV.IOVector (V3 Float) -> Int -> Int -> Int -> Int -> IO (Int, Int)
clipStuff vecs newc0 newc1 idx maxIdx
    | idx >= maxIdx = return (newc0, newc1)
    | otherwise = do
        v <- MV.read vecs idx
        s <- MV.read sides idx
        (newc0', newc1') <- calcNewC v s
        s' <- MV.read sides (idx + 1)
        doClipStuff v s s' newc0' newc1'
  where
    doClipStuff v s s' newc0' newc1'
        | s == sideOn || s' == sideOn || s == s' =
            clipStuff vecs newc0' newc1' (idx + 1) maxIdx
        | otherwise = do
            dist <- MV.read dists idx
            dist' <- MV.read dists (idx + 1)
            v' <- MV.read vecs (idx + 1)
            let d = dist / (dist - dist')
                e = v + fmap (* d) (v' - v)
            MV.write newv0 newc0' e
            MV.write newv1 newc1' e
            clipStuff vecs (newc0' + 1) (newc1' + 1) (idx + 1) maxIdx
    calcNewC :: V3 Float -> Int -> IO (Int, Int)
    calcNewC v s
        | s == sideFront = do
            MV.write newv0 newc0 v
            return (newc0 + 1, newc1)
        | s == sideBack = do
            MV.write newv1 newc1 v
            return (newc0, newc1 + 1)
        | s == sideOn = do
            MV.write newv0 newc0 v
            MV.write newv1 newc1 v
            return (newc0 + 1, newc1 + 1)
        | otherwise = error "Warp.doClipSkyPolygon shouldn't happen"

drawSkyPolygon :: Int -> MV.IOVector (V3 Float) -> Quake ()
drawSkyPolygon nump vecs = do
    error "Warp.drawSkyPolygon" -- TODO