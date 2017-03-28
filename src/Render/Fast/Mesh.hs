{-# LANGUAGE FlexibleContexts #-}
module Render.Fast.Mesh
    ( myGLUPerspective
    , rDrawAliasModel
    , rRotateForEntity
    ) where

import Text.Printf (printf)

import           Control.Lens                    (use, (^.), (+=), (&), (.~))
import           Control.Monad                   (unless, when)
import           Data.Bits                       (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Char8           as BC
import           Data.Int                        (Int32)
import           Data.IORef                      (IORef, newIORef, modifyIORef', readIORef)
import qualified Data.Vector                     as V
import qualified Data.Vector.Storable.Mutable    as MSV
import qualified Data.Vector.Unboxed             as UV
import           GHC.Float                       (float2Double)
import qualified Graphics.GL                     as GL
import           Linear                          (V3(..), dot, normalize, _x, _y, _z)

import           Client.EntityT
import           Client.RefDefT
import {-# SOURCE #-} qualified Client.VID       as VID
import qualified Constants
import           Game.CPlaneT
import           Game.CVarT
import qualified QCommon.Com                     as Com
import qualified QCommon.CVar                    as CVar
import           QCommon.CVarVariables
import           QCommon.QFiles.MD2.DMdlT
import           QCommon.QFiles.MD2.DAliasFrameT
import           QuakeRef
import           QuakeState
import           Render.Anorms
import qualified Render.Fast.Image               as Image
import qualified Render.Fast.Light               as Light
import           Render.GLStateT
import           Render.ImageT
import           Render.ModelT
import           Types
import           Util.Binary                     (encode)
import qualified Util.Math3D                     as Math3D

-- precalculated dot products for quantized angles
shadeDotQuant :: Int
shadeDotQuant = 16

myGLUPerspective :: Double -> Double -> Double -> Double -> Quake ()
myGLUPerspective fovY aspect zNear zFar = do
    glState <- use (fastRenderAPIGlobals.frGLState)
    setupPerspective glState fovY aspect zNear zFar

setupPerspective :: GLStateT -> Double -> Double -> Double -> Double -> Quake ()
setupPerspective glState fovY aspect zNear zFar =
    GL.glFrustum (realToFrac xmin')
                 (realToFrac xmax')
                 (realToFrac ymin)
                 (realToFrac ymax)
                 (realToFrac zNear)
                 (realToFrac zFar)
  where
    ymax = zNear * tan (fovY * pi / 360)
    ymin = negate ymax
    xmin = ymin * aspect
    xmax = ymax * aspect
    xmin' = xmin - (2 * float2Double (glState^.glsCameraSeparation)) / zNear
    xmax' = xmax - (2 * float2Double (glState^.glsCameraSeparation)) / zNear

rDrawAliasModel :: IORef EntityT -> Quake ()
rDrawAliasModel entityRef = do
    e <- io (readIORef entityRef)
    done <- checkIfDone e
    unless done $ do
        currentModelRef <- use (fastRenderAPIGlobals.frCurrentModel)
        maybe currentModelError (drawAliasModel entityRef) currentModelRef
  where
    checkIfDone e
        | (e^.enFlags) .&. Constants.rfWeaponModel == 0 =
            rCullAliasModel entityRef
        | otherwise = do
            handValue <- fmap (^.cvValue) handCVar
            return (handValue == 2)
    currentModelError = Com.fatalError "Mesh.rDrawAliasModel currentModelRef is Nothing"

drawAliasModel :: IORef EntityT -> Ref ModelT -> Quake ()
drawAliasModel entityRef currentModelRef = do
    currentModel <- readRef currentModelRef
    shadeLight <- buildShadeLight =<< use (fastRenderAPIGlobals.frCurrentEntity)
    doDrawAliasModel currentModel shadeLight (currentModel^.mExtraData)
  where
    doDrawAliasModel currentModel shadeLight (Just (AliasModelExtra pAliasHdr)) = do
        currentEntityRef <- maybe entityError return =<< use (fastRenderAPIGlobals.frCurrentEntity)
        currentEntity <- io (readIORef currentEntityRef)
        let idx = truncate ((currentEntity^.eAngles._y) * (fromIntegral shadeDotQuant / 360)) .&. (shadeDotQuant - 1)
            shadeDots = vertexNormalDots V.! idx
            an = (currentEntity^.eAngles._y) / 180 * pi
            shadeVector = normalize (V3 (cos (-an)) (sin (-an)) 1)
        -- locate the proper data
        fastRenderAPIGlobals.frCAliasPolys += (pAliasHdr^.dmNumTris)
        -- draw all the triangles
        when ((currentEntity^.enFlags) .&. Constants.rfDepthHack /= 0) $ do
            -- hack the depth range to prevent view model
            -- from poking into walls
            depthMin <- use (fastRenderAPIGlobals.frGLDepthMin)
            depthMax <- use (fastRenderAPIGlobals.frGLDepthMax)
            io (GL.glDepthRange (realToFrac depthMin) (realToFrac (depthMin + 0.3 * (depthMax - depthMin))))
        hand <- fmap (^.cvValue) handCVar
        when (((currentEntity^.enFlags) .&. Constants.rfWeaponModel /= 0) && hand == 1) $ do
            newRefDef <- use (fastRenderAPIGlobals.frNewRefDef)
            io $ do
                GL.glMatrixMode GL.GL_PROJECTION
                GL.glPushMatrix
                GL.glLoadIdentity
                GL.glScalef (-1) 1 1 
            myGLUPerspective (float2Double (newRefDef^.rdFovY)) (fromIntegral (newRefDef^.rdWidth) / fromIntegral (newRefDef^.rdHeight)) 4 4096
            io $ do
                GL.glMatrixMode GL.GL_MODELVIEW
                GL.glCullFace GL.GL_BACK
        io (GL.glPushMatrix)
        e <- io (readIORef entityRef)
        rRotateForEntity (e & eAngles .~ (let V3 a b c = e^.eAngles in V3 (-a) b c)) -- IMPROVE: jake2 uses PITCH instead of directly using _x
        skinRef <- getSkinRef currentModel currentEntity
        skin <- readRef skinRef
        Image.glBind (skin^.iTexNum)
        -- draw it
        io (GL.glShadeModel GL.GL_SMOOTH)
        Image.glTexEnv GL.GL_MODULATE
        when ((currentEntity^.enFlags) .&. Constants.rfTranslucent /= 0) $
            io (GL.glEnable GL.GL_BLEND)
        checkCurrentEntityFrame currentEntityRef pAliasHdr
        checkCurrentEntityOldFrame currentEntityRef pAliasHdr
        checkLerpModels currentEntityRef
        updatedCurrentEntity <- io (readIORef currentEntityRef)
        glDrawAliasFrameLerp pAliasHdr (updatedCurrentEntity^.eBackLerp) shadeLight shadeDots
        Image.glTexEnv GL.GL_REPLACE
        io $ do
            GL.glShadeModel GL.GL_FLAT
            GL.glPopMatrix
        when (((updatedCurrentEntity^.enFlags) .&. Constants.rfWeaponModel /= 0) && hand == 1) $ do
            io $ do
                GL.glMatrixMode GL.GL_PROJECTION
                GL.glPopMatrix
                GL.glMatrixMode GL.GL_MODELVIEW
                GL.glCullFace GL.GL_FRONT
        when ((updatedCurrentEntity^.enFlags) .&. Constants.rfTranslucent /= 0) $
            io (GL.glDisable GL.GL_BLEND)
        when ((updatedCurrentEntity^.enFlags) .&. Constants.rfDepthHack /= 0) $ do
            depthMin <- use (fastRenderAPIGlobals.frGLDepthMin)
            depthMax <- use (fastRenderAPIGlobals.frGLDepthMax)
            io $
                GL.glDepthRange (realToFrac depthMin) (realToFrac depthMax)
        shadows <- fmap (^.cvValue) glShadowsCVar
        when (shadows /= 0 && ((updatedCurrentEntity^.enFlags) .&. (Constants.rfTranslucent .|. Constants.rfWeaponModel) == 0)) $ do
            io (GL.glPushMatrix)
            rRotateForEntity e
            io $ do
                GL.glDisable GL.GL_TEXTURE_2D
                GL.glEnable GL.GL_BLEND
                GL.glColor4f 0 0 0 0.5
            glDrawAliasShadow pAliasHdr (updatedCurrentEntity^.eFrame) shadeVector
            io $ do
                GL.glEnable GL.GL_TEXTURE_2D
                GL.glDisable GL.GL_BLEND
                GL.glPopMatrix
        io (GL.glColor4f 1 1 1 1)
    doDrawAliasModel _ _ _ =
        Com.fatalError "Mesh.rDrawAliasModel currentModel^.mExtraData is not AliasModelExtra"
    entityError = do
        Com.fatalError "Mesh.rDrawAliasModel fastRenderAPIGlobals.frCurrentEntity is Nothing"
        newEntityRef <- io (newIORef newEntityT)
        return newEntityRef
    getSkinRef :: ModelT -> EntityT -> Quake (Ref ImageT)
    getSkinRef currentModel currentEntity =
        maybe (use (fastRenderAPIGlobals.frNoTexture)) return (getCurrentEntitySkin currentModel currentEntity (currentEntity^.eSkin))
    getCurrentEntitySkin currentModel currentEntity Nothing
        | (currentEntity^.eSkinNum) >= Constants.maxMd2Skins =
            (currentModel^.mSkins) V.! 0
        | otherwise =
            (currentModel^.mSkins) V.! (currentEntity^.eSkinNum) -- TODO: jake2 has some NULL check here as well
    getCurrentEntitySkin _ _ ref = ref
    checkCurrentEntityFrame currentEntityRef pAliasHdr = do
        currentEntity <- io (readIORef currentEntityRef)
        when ((currentEntity^.eFrame) >= (pAliasHdr^.dmNumFrames) || (currentEntity^.eFrame) < 0) $ do
            currentModel <- readRef currentModelRef
            VID.printf Constants.printAll (B.concat ["R_DrawAliasModel ", currentModel^.mName, ": no such frame ", encode (currentEntity^.eFrame), "\n"])
            io $ modifyIORef' currentEntityRef (\v -> v & eFrame .~ 0
                                                        & eOldFrame .~ 0)
    checkCurrentEntityOldFrame currentEntityRef pAliasHdr = do
        currentEntity <- io (readIORef currentEntityRef)
        when ((currentEntity^.eOldFrame) >= (pAliasHdr^.dmNumFrames) || (currentEntity^.eOldFrame) < 0) $ do
            currentModel <- readRef currentModelRef
            VID.printf Constants.printAll (B.concat ["R_DrawAliasModel ", currentModel^.mName, ": no such frame ", encode (currentEntity^.eOldFrame), "\n"])
            io $ modifyIORef' currentEntityRef (\v -> v & eFrame .~ 0
                                                        & eOldFrame .~ 0)
    checkLerpModels currentEntityRef = do
        lerpModels <- fmap (^.cvValue) lerpModelsCVar
        when (lerpModels == 0) $
            io $ modifyIORef' currentEntityRef (\v -> v & eBackLerp .~ 0)

buildShadeLight :: Maybe (IORef EntityT) -> Quake (V3 Float)
buildShadeLight Nothing = do
    Com.fatalError "Mesh.buildShadeLight currentEntityRef is Nothing"
    return (V3 0 0 0)
buildShadeLight (Just currentEntityRef) = do
    currentEntity <- io (readIORef currentEntityRef)
    newRefDef <- use $ fastRenderAPIGlobals.frNewRefDef
    fmap (applyGogglesIR currentEntity newRefDef . applyGlow currentEntity newRefDef . applyMinLight currentEntity) (buildInitialShadeLight currentEntity)

buildInitialShadeLight :: EntityT -> Quake (V3 Float)
buildInitialShadeLight currentEntity
    | (currentEntity^.enFlags) .&. (Constants.rfShellHalfDam .|. Constants.rfShellGreen .|. Constants.rfShellRed .|. Constants.rfShellBlue .|. Constants.rfShellDouble) /= 0 = do
        let (a, b, c) = if (currentEntity^.enFlags) .&. Constants.rfShellHalfDam /= 0 then (0.56, 0.59, 0.45) else (0, 0, 0)
            (a', b')  = if (currentEntity^.enFlags) .&. Constants.rfShellDouble /= 0 then (0.9, 0.7) else (a, b)
            a'' = if (currentEntity^.enFlags) .&. Constants.rfShellRed /= 0 then 1 else a'
            b'' = if (currentEntity^.enFlags) .&. Constants.rfShellGreen /= 0 then 1 else b'
            c' = if (currentEntity^.enFlags) .&. Constants.rfShellBlue /= 0 then 1 else c
        return (V3 a'' b'' c')
    | (currentEntity^.enFlags) .&. Constants.rfFullBright /= 0 =
        return (V3 1 1 1)
    | otherwise = do
        shadeLight <- Light.rLightPoint (currentEntity^.eOrigin)
        playerLightingHack currentEntity shadeLight
        monoLightmap <- fmap (^.cvString) glMonoLightMapCVar
        return (checkMonoHead shadeLight (BC.head monoLightmap))
  where
    checkMonoHead shadeLight '0' = shadeLight
    checkMonoHead shadeLight _ =
        let s = maximum shadeLight
        in V3 s s s

applyMinLight :: EntityT -> V3 Float -> V3 Float
applyMinLight currentEntity shadeLight
    | (currentEntity^.enFlags) .&. Constants.rfMinLight /= 0 =
        let v = (shadeLight^._x) > 0.1 || (shadeLight^._y) > 0.1 || (shadeLight^._z) > 0.1
        in if v then shadeLight else V3 0.1 0.1 0.1
    | otherwise = shadeLight

applyGlow :: EntityT -> RefDefT -> V3 Float -> V3 Float
applyGlow currentEntity newRefDef shadeLight
    | (currentEntity^.enFlags) .&. Constants.rfGlow /= 0 = -- bonus items will pulse with time
        let scale = 0.1 * sin (7 * (newRefDef^.rdTime))
            smin = fmap (* 0.8) shadeLight
            a = max ((shadeLight^._x) + scale) (smin^._x)
            b = max ((shadeLight^._y) + scale) (smin^._y)
            c = max ((shadeLight^._z) + scale) (smin^._z)
        in V3 a b c
    | otherwise = shadeLight

applyGogglesIR :: EntityT -> RefDefT -> V3 Float -> V3 Float
applyGogglesIR currentEntity newRefDef shadeLight
    | ((newRefDef^.rdRdFlags) .&. Constants.rdfIrGoggles /= 0) && ((currentEntity^.enFlags) .&. Constants.rfIrVisible /= 0) =
        V3 1 0 0
    | otherwise = shadeLight

-- player lighting hack for communication back to server
-- big hack!
playerLightingHack :: EntityT -> V3 Float -> Quake ()
playerLightingHack currentEntity shadeLight
    | (currentEntity^.enFlags) .&. Constants.rfWeaponModel /= 0 = do
        lightLevel <- clLightLevelCVar
        CVar.update (lightLevel & cvValue .~ (maximum shadeLight) * 150)
    | otherwise = return ()

rCullAliasModel :: IORef EntityT -> Quake Bool
rCullAliasModel entRef = do
    currentModel <- readCurrentModel
    cullAliasModel entRef currentModel (currentModel^.mExtraData)
  where
    readCurrentModel = do
        modelRef <- use (fastRenderAPIGlobals.frCurrentModel)
        maybe currentModelError readRef modelRef
    currentModelError = do
        Com.fatalError "Mesh.rCullAliasModel current model is Nothing"
        return newModelT

cullAliasModel :: IORef EntityT -> ModelT -> Maybe ModelExtra -> Quake Bool
cullAliasModel entRef currentModel (Just (AliasModelExtra pAliasHdr)) = do
    checkFrames =<< io (readIORef entRef)
    aliasFrames <- getAliasFrames (pAliasHdr^.dmAliasFrames)
    e <- io (readIORef entRef)
    let pFrame = aliasFrames V.! (e^.eFrame)
        pOldFrame = aliasFrames V.! (e^.eOldFrame)
        -- compute axially aligned mins and maxs
        (mins, maxs)
            | pFrame == pOldFrame =
                (pFrame^.dafTranslate, (pFrame^.dafTranslate) + fmap (* 255) (pFrame^.dafScale))
            | otherwise =
                let thismaxs = (pFrame^.dafTranslate) + fmap (* 255) (pFrame^.dafScale)
                    oldmaxs = (pOldFrame^.dafTranslate) + fmap (* 255) (pOldFrame^.dafScale)
                    mina = min (pFrame^.dafTranslate._x) (pOldFrame^.dafTranslate._x)
                    minb = min (pFrame^.dafTranslate._y) (pOldFrame^.dafTranslate._y)
                    minc = min (pFrame^.dafTranslate._z) (pOldFrame^.dafTranslate._z)
                    maxa = max (thismaxs^._x) (oldmaxs^._x)
                    maxb = max (thismaxs^._y) (oldmaxs^._y)
                    maxc = max (thismaxs^._z) (oldmaxs^._z)
                in (V3 mina minb minc, V3 maxa maxb maxc)
        -- compute a full bounding box
        bbox = V.generate 8 (computeBoundingBox mins maxs)
        -- rotate the bounding box
        tmp = let V3 a b c = mins
              in V3 a (negate b) c -- IMPROVE: jake2 code uses YAW from Constants
        (v0, v1, v2) = Math3D.angleVectors tmp True True True
        bbox' = fmap (rotateBoundingBox (e^.eOrigin) v0 v1 v2) bbox
        aggregateMask = complement 0 :: Int32
    frustum <- use (fastRenderAPIGlobals.frFrustum)
    let mask = computeAggregateMask frustum bbox' aggregateMask 0 8
    return (mask /= 0)
  where
    checkFrames e = do
        when ((e^.eFrame) >= (pAliasHdr^.dmNumFrames) || (e^.eFrame) < 0) $ do
            VID.printf Constants.printAll (B.concat ["R_CullAliasModel ", currentModel^.mName, ": no such frame ", encode (e^.eFrame), "\n"])
            io $ modifyIORef' entRef (\v -> v & eFrame .~ 0)
        when ((e^.eOldFrame) >= (pAliasHdr^.dmNumFrames) || (e^.eOldFrame) < 0) $ do
            VID.printf Constants.printAll (B.concat ["R_CullAliasModel ", currentModel^.mName, ": no such oldframe ", encode (e^.eOldFrame), "\n"])
            io $ modifyIORef' entRef (\v -> v & eOldFrame .~ 0)
    getAliasFrames Nothing = do
        Com.fatalError "Mesh.rCullAliasModel pAliasHdr^.dmAliasFrames is Nothing"
        return V.empty
    getAliasFrames (Just aliasFrames) =
        return aliasFrames
cullAliasModel _ _ _ = do
    Com.fatalError "Mesh.rCullAliasModel currentModel^.mExtraData is not AliasModelExtra"
    return False

computeBoundingBox :: V3 Float -> V3 Float -> Int -> V3 Float
computeBoundingBox mins maxs idx =
    let a = if idx .&. 1 /= 0 then mins^._x else maxs^._x
        b = if idx .&. 2 /= 0 then mins^._y else maxs^._y
        c = if idx .&. 4 /= 0 then mins^._z else maxs^._z
    in V3 a b c

rotateBoundingBox :: V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float -> V3 Float
rotateBoundingBox origin v0 v1 v2 tmp =
    let a = v0 `dot` tmp
        b = v1 `dot` tmp
        c = v2 `dot` tmp
    in (V3 a b c) + origin

computeAggregateMask :: V.Vector CPlaneT -> V.Vector (V3 Float) -> Int32 -> Int -> Int -> Int32
computeAggregateMask frustum bbox aggregateMask idx maxIdx
    | idx >= maxIdx = aggregateMask
    | otherwise =
        let mask = computeMask frustum (bbox V.! idx) 0 0 4
        in computeAggregateMask frustum bbox (aggregateMask .&. mask) (idx + 1) maxIdx

computeMask :: V.Vector CPlaneT -> V3 Float -> Int32 -> Int -> Int -> Int32
computeMask frustum bbox mask idx maxIdx
    | idx >= maxIdx = mask
    | otherwise =
        let dp = ((frustum V.! idx)^.cpNormal) `dot` bbox
        in if dp - ((frustum V.! idx)^.cpDist) < 0
             then computeMask frustum bbox (mask .|. (1 `shiftL` idx)) (idx + 1) maxIdx
             else computeMask frustum bbox mask (idx + 1) maxIdx

rRotateForEntity :: EntityT -> Quake ()
rRotateForEntity e = do
    let origin = fmap realToFrac (e^.eOrigin)
        angles = fmap realToFrac (e^.eAngles)
    io $ do
        GL.glTranslatef (origin^._x) (origin^._y) (origin^._z)
        GL.glRotatef         (angles^._y)  0 0 1
        GL.glRotatef (negate (angles^._x)) 0 1 0
        GL.glRotatef (negate (angles^._z)) 1 0 0

glDrawAliasShadow :: DMdlT -> Int -> V3 Float -> Quake ()
glDrawAliasShadow = error "Mesh.glDrawAliasShadow" -- TODO

glDrawAliasFrameLerp :: DMdlT -> Float -> V3 Float -> UV.Vector Float -> Quake ()
glDrawAliasFrameLerp pAliasHdr backLerp shadeLight shadeDots = do
    currentEntityRef <- maybe entityError return =<< use (fastRenderAPIGlobals.frCurrentEntity)
    currentEntity <- io (readIORef currentEntityRef)
    aliasFrames <- getAliasFrames (pAliasHdr^.dmAliasFrames)
    let frame = aliasFrames V.! (currentEntity^.eFrame)
        oldFrame = aliasFrames V.! (currentEntity^.eOldFrame)
        alpha | (currentEntity^.enFlags) .&. Constants.rfTranslucent /= 0 = currentEntity^.eAlpha
              | otherwise                                                 = 1.0
        frontLerp = 1.0 - backLerp
        -- move should be the delta back to the previous frame * backlerp
        frontV = (currentEntity^.eOldOrigin) - (currentEntity^.eOrigin)
        (v0, v1, v2) = Math3D.angleVectors (currentEntity^.eAngles) True True True
        move = (V3 (frontV `dot` v0) (negate (frontV `dot` v1)) (frontV `dot` v2)) + (oldFrame^.dafTranslate)
        move' = fmap (* backLerp) move + fmap (* frontLerp) (frame^.dafTranslate)
        frontV' = fmap (* frontLerp) (frame^.dafScale)
        backV' = fmap (* backLerp) (oldFrame^.dafScale)
    -- PMM - added double shell
    when ((currentEntity^.enFlags) .&. (Constants.rfShellRed .|. Constants.rfShellGreen .|. Constants.rfShellBlue .|. Constants.rfShellDouble .|. Constants.rfShellHalfDam) /= 0) $
        io (GL.glDisable GL.GL_TEXTURE_2D)
    glLerpVerts (pAliasHdr^.dmNumXYZ) (oldFrame^.dafVerts) (frame^.dafVerts) move' frontV' backV'
    vertexArrayBuf <- use (fastRenderAPIGlobals.frVertexArrayBuf)
    io $ do
        GL.glEnableClientState GL.GL_VERTEX_ARRAY
        MSV.unsafeWith vertexArrayBuf $ \ptr ->
            GL.glVertexPointer 3 GL.GL_FLOAT 0 ptr
    addShellColor currentEntity frame alpha
    texture0 <- use (fastRenderAPIGlobals.frTexture0)
    textureArrayBuf <- use (fastRenderAPIGlobals.frTextureArrayBuf)
    srcTextureCoords <- use (fastRenderAPIGlobals.frModelTextureCoordBuf)
    vertexIndexBuf <- use (fastRenderAPIGlobals.frModelVertexIndexBuf)
    io $ do
        GL.glClientActiveTextureARB (fromIntegral texture0)
        MSV.unsafeWith textureArrayBuf $ \ptr ->
            GL.glTexCoordPointer 2 GL.GL_FLOAT 0 ptr
        GL.glEnableClientState GL.GL_TEXTURE_COORD_ARRAY -- TODO? jake2 has this commented out?
        -- DEBUG
        -- putStrLn "ksaveljev drawAlias"
        -- mapM_ (\i -> do
        --           v <- MSV.read srcTextureCoords i
        --           putStr $ printf "%.2f " v) [0..100]
        -- putStrLn ""
        drawElements textureArrayBuf vertexIndexBuf srcTextureCoords 0 0 (UV.length (pAliasHdr^.dmCounts))
        when ((currentEntity^.enFlags) .&. (Constants.rfShellRed .|. Constants.rfShellGreen .|. Constants.rfShellBlue .|. Constants.rfShellDouble .|. Constants.rfShellHalfDam) /= 0) $
            GL.glEnable GL.GL_TEXTURE_2D
        GL.glDisableClientState GL.GL_COLOR_ARRAY
  where
    entityError = do
        Com.fatalError "Mesh.glDrawAliasFrameLerp fastRenderAPIGlobals.frCurrentEntity is Nothing"
        newEntityRef <- io (newIORef newEntityT)
        return newEntityRef
    getAliasFrames Nothing = do
        Com.fatalError "Mesh.glDrawAliasFrameLerp pAliasHdr^.dmAliasFrames is Nothing"
        return V.empty
    getAliasFrames (Just aliasFrames) =
        return aliasFrames
    addShellColor currentEntity frame alpha
        | (currentEntity^.enFlags) .&. (Constants.rfShellRed .|. Constants.rfShellGreen .|. Constants.rfShellBlue .|. Constants.rfShellDouble .|. Constants.rfShellHalfDam) /= 0 =
            io (GL.glColor4f (realToFrac (shadeLight^._x)) (realToFrac (shadeLight^._y)) (realToFrac (shadeLight^._z)) (realToFrac alpha))
        | otherwise = do
            colorArrayBuf <- use (fastRenderAPIGlobals.frColorArrayBuf)
            io $ do
                GL.glEnableClientState GL.GL_COLOR_ARRAY
                MSV.unsafeWith colorArrayBuf $ \ptr ->
                    GL.glColorPointer 4 GL.GL_FLOAT 0 ptr
                -- pre light everything
                prelight colorArrayBuf (frame^.dafVerts) alpha 0 0 (pAliasHdr^.dmNumXYZ)
    prelight :: MSV.IOVector Float -> UV.Vector Int -> Float -> Int -> Int -> Int -> IO ()
    prelight colorArrayBuf verts alpha j idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            let l = shadeDots UV.! (((verts UV.! idx) `shiftR` 24) .&. 0xFF)
            MSV.write colorArrayBuf (j + 0) (l * (shadeLight^._x))
            MSV.write colorArrayBuf (j + 1) (l * (shadeLight^._y))
            MSV.write colorArrayBuf (j + 2) (l * (shadeLight^._z))
            MSV.write colorArrayBuf (j + 3) alpha
            prelight colorArrayBuf verts alpha (j + 4) (idx + 1) maxIdx
    drawElements :: MSV.IOVector Float -> MSV.IOVector Int32 -> MSV.IOVector Float -> Int -> Int -> Int -> IO ()
    drawElements textureArrayBuf vertexIndexBuf srcTextureCoords pos idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            let count = (pAliasHdr^.dmCounts) UV.! idx
            unless (count == 0) $ do
                let (indexStartIdx, indexLength) = (pAliasHdr^.dmIndexElements) V.! idx
                    (mode, count')
                        | count < 0 = (GL.GL_TRIANGLE_FAN, negate count)
                        | otherwise = (GL.GL_TRIANGLE_STRIP, count)
                    srcIndex = (pos `shiftL` 1) - 1
                    buf = MSV.slice indexStartIdx indexLength vertexIndexBuf
                addTextureCoords textureArrayBuf buf srcTextureCoords srcIndex 0 (fromIntegral count')
                MSV.unsafeWith buf $ \ptr ->
                    GL.glDrawElements mode (fromIntegral indexLength) GL.GL_UNSIGNED_INT ptr
                drawElements textureArrayBuf vertexIndexBuf srcTextureCoords (pos + fromIntegral count') (idx + 1) maxIdx
    addTextureCoords :: MSV.IOVector Float -> MSV.IOVector Int32 -> MSV.IOVector Float -> Int -> Int -> Int -> IO ()
    addTextureCoords textureArrayBuf srcIndexBuf srcTextureCoords srcIndex idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            dstIndex <- MSV.read srcIndexBuf idx
            let dstIndex' = dstIndex `shiftL` 1
            a <- MSV.read srcTextureCoords ((pAliasHdr^.dmTextureCoordBufIdx) + (srcIndex + 1))
            b <- MSV.read srcTextureCoords ((pAliasHdr^.dmTextureCoordBufIdx) + (srcIndex + 2))
            MSV.write textureArrayBuf (fromIntegral dstIndex' + 0) a
            MSV.write textureArrayBuf (fromIntegral dstIndex' + 1) b
            addTextureCoords textureArrayBuf srcIndexBuf srcTextureCoords (srcIndex + 2) (idx + 1) maxIdx

-- TODO: we do not recreate vertexArrayBuf like in second_attempt branch... has to be researched if it is ok
glLerpVerts :: Int -> UV.Vector Int -> UV.Vector Int -> V3 Float -> V3 Float -> V3 Float -> Quake ()
glLerpVerts nVerts ov v move frontV backV = do
    currentEntityRef <- maybe entityError return =<< use (fastRenderAPIGlobals.frCurrentEntity)
    currentEntity <- io (readIORef currentEntityRef)
    vertexArrayBuf <- use (fastRenderAPIGlobals.frVertexArrayBuf)
    io (updateLerp vertexArrayBuf currentEntity)
  where
    entityError = do
        Com.fatalError "Mesh.glLerpVerts fastRenderAPIGlobals.frCurrentEntity is Nothing"
        newEntityRef <- io (newIORef newEntityT)
        return newEntityRef
    updateLerp vertexArrayBuf currentEntity
        | (currentEntity^.enFlags) .&. (Constants.rfShellRed .|. Constants.rfShellGreen .|. Constants.rfShellBlue .|. Constants.rfShellDouble .|. Constants.rfShellHalfDam) /= 0 =
            updateLerp1 vertexArrayBuf 0 0 nVerts
        | otherwise =
            updateLerp2 vertexArrayBuf 0 0 nVerts
    updateLerp1 vertexArrayBuf j idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            let vv = v UV.! idx
                ovv = ov UV.! idx
                normal = vertexNormals V.! ((vv `shiftR` 24) .&. 0xFF)
            MSV.write vertexArrayBuf (j + 0) ((move^._x) + fromIntegral (ovv .&. 0xFF) * (backV^._x) + fromIntegral (vv .&. 0xFF) * (frontV^._x) + (normal^._x) * Constants.powersuitScale)
            MSV.write vertexArrayBuf (j + 1) ((move^._y) + fromIntegral ((ovv `shiftR` 8) .&. 0xFF) * (backV^._y) + fromIntegral ((vv `shiftR` 8) .&. 0xFF) * (frontV^._y) + (normal^._y) * Constants.powersuitScale)
            MSV.write vertexArrayBuf (j + 2) ((move^._z) + fromIntegral ((ovv `shiftR` 16) .&. 0xFF) * (backV^._z) + fromIntegral ((vv `shiftR` 16) .&. 0xFF) * (frontV^._z) + (normal^._z) * Constants.powersuitScale)
            updateLerp1 vertexArrayBuf (j + 3) (idx + 1) maxIdx
    updateLerp2 vertexArrayBuf j idx maxIdx
        | idx >= maxIdx = return ()
        | otherwise = do
            let ovv = ov UV.! idx
                vv = v UV.! idx
            MSV.write vertexArrayBuf (j + 0) ((move^._x) + fromIntegral (ovv .&. 0xFF) * (backV^._x) + fromIntegral (vv .&. 0xFF) * (frontV^._x))
            MSV.write vertexArrayBuf (j + 1) ((move^._y) + fromIntegral ((ovv `shiftR` 8) .&. 0xFF) * (backV^._y) + fromIntegral ((vv `shiftR` 8) .&. 0xFF) * (frontV^._y))
            MSV.write vertexArrayBuf (j + 2) ((move^._z) + fromIntegral ((ovv `shiftR` 16) .&. 0xFF) * (backV^._z) + fromIntegral ((vv `shiftR` 16) .&. 0xFF) * (frontV^._z))
            updateLerp2 vertexArrayBuf (j + 3) (idx + 1) maxIdx
